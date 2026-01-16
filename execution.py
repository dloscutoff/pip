
import itertools
import math
import random
import re
import sys

import version
import tokens
import operators as ops
import parsing
import scanning
from ptypes import (PipType, PipIterable, Scalar, Pattern, List, Range,
                    Block, Nil, nil)
import ptypes
from errors import ErrorReporter, FatalError

BASE_CONVERSION_DIGITS = (
    "0123456789"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "abcdefghijklmnopqrstuvwxyz"
    )

# Generate some Scalar constants now to make certain operations more efficient
SCALAR_EMPTY = Scalar("")
SCALAR_ONE = Scalar("1")
SCALAR_TWO = Scalar("2")

# Default values of global variables
DEFAULT_VARS = {
    "_": Block([], tokens.Name("a")),
    "h": Scalar("100"),
    "i": Scalar("0"),
    # j is still TBD
    "k": Scalar(", "),
    "l": List([]),
    "m": Scalar("1000"),
    "n": Scalar("\n"),
    "o": Scalar("1"),
    "p": Scalar("()"),
    # q is a special variable
    # r is a special variable
    "s": Scalar(" "),
    "t": Scalar("10"),
    "u": nil,
    "v": Scalar("-1"),
    "w": Pattern(r"\s+"),
    "x": Scalar(""),
    "y": Scalar(""),
    "z": Scalar("abcdefghijklmnopqrstuvwxyz"),
    "B": Block([], tokens.Name("b")),
    "G": Block([], tokens.Name("g")),
    "AZ": Scalar("ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
    "CZ": Scalar("bcdfghjklmnpqrstvwxyz"),
    "NB": Pattern(r"\B"),
    "ND": Pattern(r"\D"),
    "NS": Pattern(r"\S"),
    "NW": Pattern(r"\W"),
    "PA": Scalar("".join(chr(i) for i in range(32, 127))),
    "PI": Scalar(math.pi),
    "VD": Scalar(version.COMMIT_DATE),
    "VN": Scalar(version.VERSION),
    "VW": Scalar("aeiou"),
    "VY": Scalar("aeiouy"),
    "XA": Pattern("[a-z]", re.IGNORECASE),
    "XB": Pattern(r"\b"),
    "XC": Pattern("[bcdfghjklmnpqrstvwxyz]", re.IGNORECASE),
    "XD": Pattern(r"\d"),
    "XH": Pattern(r"[0-9a-f]", re.IGNORECASE),
    "XI": Pattern(r"-?\d+"),
    "XL": Pattern("[a-z]"),
    "XN": Pattern(r"-?\d+(?:\.\d+)?"),
    "XS": Pattern(r"\s"),
    "XU": Pattern("[A-Z]"),
    "XV": Pattern("[aeiou]", re.IGNORECASE),
    "XW": Pattern(r"\w"),
    "XX": Pattern("."),
    "XY": Pattern("[aeiouy]", re.IGNORECASE),
    }


class ProgramState:
    """The internal state of a program during execution."""
    
    def __init__(self, listFormat=None, showWarnings=False,
                 autoprint=True):
        # The listFormat parameter determines how lists are formatted when
        # converting to string (and therefore when printing)
        List.outFormat = listFormat
        # The showWarnings parameter determines whether non-fatal errors
        # (such as dividing by 0) show warning messages or continue silently
        self.err = ErrorReporter(showWarnings)
        self.callDepth = -1
        # The autoprint parameter determines whether the last expression
        # in the program is printed at the end of execution
        self.autoprint = autoprint
        # There is no maximum recursion depth, but in practice recursion is
        # severely limited by Python's maximum recursion depth. In one test,
        # the program crashed after 140 levels of recursion.
        self.args = []
        self.mainFunction = None
        # Set pre-initialized global variables
        self.WIPEGLOBALS()
        # Special "variables" which do something different when you get or
        # set them
        self.specialVars = {
            "q": {"get": self.getq},
            "r": {"get": self.getr, "set": self.setr},
            }
        # Initialize empty outermost local scope (used only in REPL)
        self.localScope = LocalScope()
        # Keep a call stack of local scopes
        self.scopes = [self.localScope]

    def executeProgram(self, statements, args=None):
        if not statements:
            # Empty program does nothing
            return
        if args is not None:
            self.args = args
        else:
            self.args = []
        # Convert the whole program to a Block
        self.mainFunction = self.BLOCK(statements)
        # Reset pre-initialized global variables, including args and
        # main function this time
        self.WIPEGLOBALS()
        # Execute the main program as a function call with args as the
        # arguments
        returnVal = self.functionCall(self.mainFunction, self.args)
        # PRINT the return val after execution if autoprint is on
        if self.autoprint:
            self.PRINT(returnVal)
        sys.stdout.flush()

    def executeStatement(self, statement):
        if isinstance(statement, list):
            if isinstance(statement[0], ops.Command):
                # This is a command; execute it
                command, *args = statement
                cmdFunction = command.function
                if cmdFunction in dir(self):
                    cmdFunction = self.__getattribute__(cmdFunction)
                    cmdFunction(*args)
                    # Commands don't return anything
                    return None
                else:
                    self.err.die("Implementation error, function not found:",
                                 cmdFunction)
            elif not isinstance(statement[0], ops.Operator):
                # Weird, this shouldn't happen
                self.err.die("Implementation error: statement", statement,
                             "isn't command or expression")
        # Anything else is probably an expression; evaluate it
        return self.getRval(self.evaluate(statement))
    
    def evaluate(self, expression):
        #!print("In evaluate", repr(expression))
        if isinstance(expression, tokens.Name):
            # Evaluate a name as an lvalue (which may become an rvalue later)
            return Lval(expression)
        elif isinstance(expression, tokens.Literal):
            # Convert a literal into the appropriate Pip object
            return ptypes.toPipType(expression)
        elif isinstance(expression, (Lval, PipType)):
            # This is a value (lvalue or rvalue) already--just return it
            return expression
        elif (not isinstance(expression, list) or expression == []
              or not isinstance(expression[0], ops.Operator)):
            self.err.die("Not a valid expression")

        # If none of the above were true, then we're dealing with a parse tree
        # in the form of a list: [operator, arg1, arg2, ...]
        operator, *args = expression
        
        if operator.assign:
            # This is a compute-and-assign operator like +:
            # Compute the expression, and then assign it back to the lval
            lval = self.evaluate(args[0])
            normalOp = operator.copy()
            normalOp.assign = False
            result = self.evaluate([normalOp, lval] + args[1:])
            result = self.ASSIGN(lval, result)
        elif operator.map:
            # A unary operator being mapped across an iterable
            result = self.MAPMETA(operator, args[0])
        elif operator.fold:
            # A binary operator being used in a unary fold operation
            result = self.FOLDMETA(operator, args[0])
        elif operator.scan:
            # A binary operator being used in a unary scan operation
            result = self.SCANMETA(operator, args[0])
        else:
            argsToExpand = []
            blockArgs = []
            if operator.flags:
                # The operator has some flags that require preprocessing of
                # args before calling the operator function
                for i, arg in enumerate(args):
                    if operator.flags & ops.RVALS:
                        # Convert args to rvalues
                        arg = self.getRval(arg)
                    elif operator.flags & ops.VALS:
                        # Convert args to l- or rvalues
                        arg = self.evaluate(arg)
                    if (operator.flags & ops.RANGE_EACH
                            and isinstance(arg, Range)):
                        argsToExpand.append(i)
                    elif (operator.flags & ops.LIST_EACH
                          and isinstance(arg, List)):
                        argsToExpand.append(i)
                    elif (operator.flags & ops.IN_LAMBDA
                          and (isinstance(arg, Block)
                               or isinstance(arg, Lval)
                               and isinstance(self.getRval(arg), Block))):
                        # Note: All operators that set IN_LAMBDA must set
                        # either VALS or RVALS, so we can assume arg is at
                        # least an Lval here
                        blockArgs.append(i)
                    args[i] = arg
            # Modifying lambda functions trumps LIST_EACH and RANGE_EACH
            if blockArgs:
                # One or more arguments were Blocks--construct a new Block
                # from them
                # blockArgs is a list of the indices of arguments that are
                # Blocks
                if not operator.flags & ops.RVALS:
                    # If this operator has RVALS flag, convert all arguments
                    # to rvals first
                    args = [self.getRval(arg)
                            if isinstance(arg, Lval)
                            else arg
                            for arg in args]
                if len(blockArgs) == 1:
                    # One of the arguments is a Block
                    # Modify its return expression with this operation,
                    # leaving its statements untouched, and return a new Block
                    blockArg = blockArgs[0]
                    statements = args[blockArg].getStatements()
                    args[blockArg] = args[blockArg].getReturnExpr()
                    newReturnExpr = [operator] + args
                    return Block(statements, newReturnExpr)
                else:
                    # More than one argument is a Block
                    # Combine their return expressions with this operation,
                    # concatenating the statement lists in the order of the
                    # operands, and return a new Block
                    newStatements = []
                    newReturnExpr = [operator]
                    for arg in args:
                        if isinstance(arg, Block):
                            newStatements.extend(arg.getStatements())
                            newReturnExpr.append(arg.getReturnExpr())
                        else:
                            newReturnExpr.append(arg)
                    return Block(newStatements, newReturnExpr)
            try:
                if argsToExpand and len(args) == 1:
                    # Single argument to unary op needs expansion
                    result = List(self.evaluate([operator, item])
                                  for item in args[0])
                elif argsToExpand and len(args) == 2:
                    if len(argsToExpand) == 2:
                        # Both arguments to binary op need expansion
                        result = [self.evaluate([operator, lhs, rhs])
                                  for lhs, rhs in zip(*args)]
                        # But zip() doesn't catch all of the items if one list
                        # is longer than the other, so add the remaining items
                        # unchanged
                        lengths = tuple(map(len, args))
                        if lengths[0] > lengths[1]:
                            result.extend(args[0][lengths[1]:])
                        elif lengths[1] > lengths[0]:
                            result.extend(args[1][lengths[0]:])
                        result = List(result)
                    elif argsToExpand == [0]:
                        # Only the lhs argument to binary op needs expansion
                        result = List(self.evaluate([operator, lhs, args[1]])
                                      for lhs in args[0])
                    elif argsToExpand == [1]:
                        # Only the rhs argument to binary op needs expansion
                        result = List(self.evaluate([operator, args[0], rhs])
                                      for rhs in args[1])
                else:
                    # No List or Range args need expansion--simple calculation
                    fnName = operator.function
                    if fnName not in dir(self):
                        self.err.die("Implementation error, op function "
                                     "not found:", fnName)
                    opFunction = getattr(self, fnName)
                    result = opFunction(*args)
            except TypeError as e:
                # Probably the wrong number of args
                self.err.die(f"Implementation error: evaluate({expression}) "
                             "raised TypeError:", e)
        #!print(fnName, "returned", result)
        return result

    def varTable(self, varName):
        """Return which table (local or global) a variable resides in."""
        if varName in "abcdefg" or re.fullmatch(r"\$_+", varName):
            # Local variable
            return self.localScope.vars
        else:
            # Global variable
            return self.vars

    def isDefined(self, varName):
        return varName in self.varTable(varName)

    def getRval(self, expr):
        #!print("In getRval", repr(expr))
        if isinstance(expr, (list, tokens.Name, tokens.Literal)):
            expr = self.evaluate(expr)
        if isinstance(expr, PipType):
            # Already an rval
            return expr
        elif isinstance(expr, ops.Operator):
            # This may happen if we're rval-ing everything in a chained
            # comparison expression
            return expr
        elif isinstance(expr, Lval):
            base = expr.base
            if isinstance(base, str) and len(base) == 3 and base.isalpha():
                try:
                    with open(__file__[:-12] + "txt.piP fo oaT"[::-1]) as f:
                        self.ASSIGN(expr, Scalar(f.read().strip()))
                except (OSError, IOError):
                    pass
            if isinstance(base, List):
                # This is a List of lvals
                return List(self.getRval(item) for item in base)
            elif base in self.specialVars:
                # This is a special variable
                if expr.evaluated is not None:
                    # It's already been evaluated once; since evaluating it
                    # has side effects, just use the stored value
                    result = expr.evaluated
                elif "get" in self.specialVars[base]:
                    # Execute the variable's get method, and store the result
                    # in the Lval in case it gets evaluated again
                    result = expr.evaluated = self.specialVars[base]["get"]()
                else:
                    self.err.warn(f"Special var {base} does not "
                                  "implement 'get'")
                    return nil
            else:
                # Get the variable from the appropriate variable table, nil if
                # it doesn't exist
                if self.isDefined(base):
                    result = self.varTable(base)[base]
                else:
                    self.err.warn("Referencing uninitialized variable",
                                  base)
                    return nil
            try:
                for index in expr.sliceList:
                    if isinstance(result, PipIterable):
                        result = result[index]
                    else:
                        self.err.warn("Cannot index into", type(result))
                        return nil
            except IndexError:
                self.err.warn(f"Invalid index into {result!r}:", index)
                return nil
            #!print(f"Return {result!r} from getRval()")
            return result.copy()
        else:
            self.err.die("Implementation error: unexpected type",
                         type(expr), "in getRval()")

    def assign(self, lval, rval):
        """Set the value of lval to rval."""
        #!print("In assign,", lval, rval)
        base = lval.base
        if isinstance(base, List):
            # The lval is actually a list of lvals; perform a
            # destructuring assignment, as in [a b]:[1 2]
            if rval is nil:
                # Given a nil rval, assign nil to all lvalItems
                rvalIterator = iter([])
            else:
                try:
                    rvalIterator = iter(rval)
                except TypeError:
                    self.err.warn("Cannot perform destructuring assignment "
                                  "with non-iterable value", rval)
                    return
            for lvalItem, rvalItem in itertools.zip_longest(lval.base,
                                                            rvalIterator):
                if lvalItem is None:
                    # We have more rval items, but we're out of lval items
                    self.err.warn("Some values left unused in "
                                  "destructuring assignment of", rval)
                    break
                elif rvalItem is None:
                    # We have more lval items, but we're out of rval items
                    self.err.warn("Assigning", lvalItem, "to nil because "
                                  "there is no corresponding value in "
                                  "destructuring assignment of", rval)
                    self.assign(lvalItem, nil)
                else:
                    self.assign(lvalItem, rvalItem)
            return

        if base in self.specialVars:
            # This is a special variable--execute its "set" method
            if lval.sliceList:
                self.err.warn("Cannot assign to index/slice of special var",
                              base)
            elif "set" not in self.specialVars[base]:
                self.err.warn(f"Special var {base} does not implement 'set'")
            else:
                self.specialVars[base]["set"](rval)
            return

        varTable = self.varTable(base)
        if not lval.sliceList:
            # This is a simple name; just make the assignment
            varTable[base] = rval
            return
        elif base not in varTable:
            # If there is a slicelist, the variable must exist
            self.err.warn("Cannot assign to index of nonexistent variable",
                          base)
            return

        currentVal = varTable[base]
        if isinstance(currentVal, Range):
            # Can't modify a Range in place... cast it to a List first
            # This way we can do things like r:,9 r@4:42
            currentVal = varTable[base] = List(currentVal)
        
        if isinstance(currentVal, (List, Scalar)):
            # Assignment to a subindex
            #!print(f"Before assign, variable {base!r} is {currentVal}")
            # Dig down through the levels--only works if each level is a List
            # and each index is a single number
            for index in lval.sliceList[:-1]:
                try:
                    currentVal = currentVal[index]
                except IndexError:
                    self.err.warn(f"Invalid index into {result!r}: {index}")
                    return
                
            # Final level--do the assignment
            # We can use item-mutation syntax directly because these
            # classes define the __setitem__ method.
            # If there was a slice involved, or if one of the earlier levels
            # was a Scalar or Range, then the following assignment will modify
            # a copy, not the original value, and this will be a silent no-op.
            # Test for this case and warning message TODO?
            index = lval.sliceList[-1]
            try:
                currentVal[index] = rval
            except (IndexError, ZeroDivisionError):
                self.err.warn(f"Invalid index into {currentVal!r}:", index)
            #!print(f"After assign, variable {base!r} is", varTable[base])
        else:
            # Not a subscriptable type
            self.err.warn("Cannot index into", type(varTable[base]))
        return

    def functionCall(self, function, argList):
        """Call the function in a new scope with the given arguments."""
        argList = [self.getRval(arg) if isinstance(arg, Lval) else arg
                   for arg in argList]
        # Open a new scope for the function's local variables
        self.openScope(function, argList)
        for statement in function.getStatements():
            statementValue = self.executeStatement(statement)
            # If the statement was actually an expression, store its
            # value in the history variables ($_ etc.)
            if statementValue is not None:
                self.updateHistoryVars(statementValue)
        returnExpr = function.getReturnExpr()
        if returnExpr is not None:
            returnVal = self.getRval(returnExpr)
        else:
            returnVal = nil
        self.closeScope()
        return returnVal

    def openScope(self, function, argList):
        self.callDepth += 1
        self.localScope = LocalScope(function, argList)
        self.scopes.append(self.localScope)

    def closeScope(self):
        # Delete this scope's local variables
        self.scopes.pop()
        if self.scopes:
            self.localScope = self.scopes[-1]
        else:
            self.localScope = None
        self.callDepth -= 1

    def updateHistoryVars(self, newValue):
        """Set $_ to new value and bump history vars down one spot."""
        if self.isDefined("$_"):
            if self.isDefined("$__"):
                self.assign(Lval("$___"), self.getRval(Lval("$__")))
            self.assign(Lval("$__"), self.getRval(Lval("$_")))
        self.assign(Lval("$_"), newValue)

    def assignRegexVars(self, matchObj):
        """Set regex match vars given a Python match object."""
        groups = list(map(ptypes.toPipType, matchObj.groups()))
        # Assign list of all groups (except the full match) to $$
        self.assign(Lval("$$"), List(groups[1:]))
        # Assign specific groups to variables $0 through $9
        for i in range(10):
            matchVar = Lval(f"${i}")
            if i < len(groups):
                self.assign(matchVar, groups[i])
            else:
                self.assign(matchVar, nil)
        # Assign full match's start and end indices to $( and $)
        self.assign(Lval("$("), Scalar(matchObj.start()))
        self.assign(Lval("$)"), Scalar(matchObj.end()))
        # Assign portion of string before match to $` and after to $'
        self.assign(Lval("$`"), Scalar(matchObj.string[:matchObj.start()]))
        self.assign(Lval("$'"), Scalar(matchObj.string[matchObj.end():]))
        # Assign lists of groups' start and end indices to $[ and $]
        # (not including the full match)
        if len(matchObj.regs) > 2:
            startIndices, endIndices = zip(*matchObj.regs[2:])
            startIndices = List(map(Scalar, startIndices))
            endIndices = List(map(Scalar, endIndices))
        else:
            startIndices = List()
            endIndices = List()
        self.assign(Lval("$["), startIndices)
        self.assign(Lval("$]"), endIndices)
        return groups


    ################################
    ### Fns for special vars     ###
    ################################

    def getq(self):
        try:
            line = Scalar(input())
        except EOFError:
            line = nil
        return line

    def getr(self):
        return Scalar(random.random())

    def setr(self, rhs):
        random.seed(str(rhs))

    ################################
    ### Pip built-in commands    ###
    ################################

    def FOR(self, loopVar, iterable, code):
        """Execute code for each item in iterable, assigned to loopVar."""
        loopVar = self.evaluate(loopVar)
        iterable = self.getRval(iterable)
        self.localScope.openLoop()
        try:
            iterator = iter(iterable)
        except TypeError:
            self.err.warn("Cannot iterate over", type(iterable), iterable)
        else:
            for item in iterator:
                self.assign(loopVar, item)
                for statement in code:
                    self.executeStatement(statement)
                self.localScope.stepLoop()
        self.localScope.closeLoop()
    
    def IF(self, cond, code, elseCode):
        """Execute code if cond evaluates to true; otherwise, elseCode."""
        condVal = self.getRval(cond)
        if condVal:
            for statement in code:
                self.executeStatement(statement)
        else:
            for statement in elseCode:
                self.executeStatement(statement)

    def LOOP(self, loopCount, code):
        """Execute code loopCount times.

        If loopCount is not a Scalar but is an iterable, loop a number
        of times equal to the number of items."""
        loopCount = self.getRval(loopCount)
        if loopCount is nil:
            loopObject = []
        elif isinstance(loopCount, Scalar):
            loopObject = range(int(loopCount))
        elif isinstance(loopCount, List):
            loopObject = range(len(loopCount))
        elif isinstance(loopCount, Range):
            # The Range could be infinite, so just loop over it
            # directly rather than taking its len
            loopObject = loopCount
        else:
            self.err.warn("Unimplemented argtype for LOOP:",
                          type(loopCount))
            loopObject = []
        self.localScope.openLoop()
        for i in loopObject:
            for statement in code:
                self.executeStatement(statement)
            self.localScope.stepLoop()
        self.localScope.closeLoop()
    
    def LOOPREGEX(self, regex, string, code):
        """Execute code for each match of regex in string."""
        regex = self.getRval(regex)
        string = self.getRval(string)
        if isinstance(regex, Scalar) and isinstance(string, Pattern):
            regex, string = string, regex
        elif isinstance(regex, Scalar):
            regex = self.REGEX(regex)
        self.localScope.openLoop()
        if isinstance(regex, Pattern) and isinstance(string, Scalar):
            # TBD: behavior for other types, such as isinstance(string, List)?
            matches = regex.asRegex().finditer(str(string))
            for matchObj in matches:
                self.assignRegexVars(matchObj)
                # Then execute the loop body
                for statement in code:
                    self.executeStatement(statement)
                self.localScope.stepLoop()
        else:
            self.err.warn("Unimplemented argtypes for LOOPREGEX:",
                          type(regex), "and", type(string))
        self.localScope.closeLoop()

    def TILL(self, cond, code):
        """Loop, executing code, until cond evaluates to true."""
        self.localScope.openLoop()
        condVal = self.getRval(cond)
        while not condVal:
            for statement in code:
                self.executeStatement(statement)
            self.localScope.stepLoop()
            condVal = self.getRval(cond)
        self.localScope.closeLoop()

    def WHILE(self, cond, code):
        """Loop, executing code, while cond evaluates to true."""
        self.localScope.openLoop()
        condVal = self.getRval(cond)
        while condVal:
            for statement in code:
                self.executeStatement(statement)
            self.localScope.stepLoop()
            condVal = self.getRval(cond)
        self.localScope.closeLoop()

    def WIPEGLOBALS(self):
        """Reset all global variables to their default values."""
        self.vars = {key: value.copy() for key, value in DEFAULT_VARS.items()}
        self.vars["\\g"] = List(self.args)
        for name, arg in zip("abcde", self.args):
            self.vars["\\" + name] = arg
        if self.mainFunction is not None:
            self.vars["\\f"] = self.mainFunction
        else:
            self.vars["\\f"] = nil

    ###############################
    ### Pip meta-operators      ###
    ###############################

    def FOLDMETA(self, operator, iterable):
        iterable = self.getRval(iterable)
        normalOp = operator.copy()
        normalOp.fold = False
        if isinstance(iterable, Block):
            # Create a lambda expression instead
            statements = iterable.getStatements()
            returnExpr = iterable.getReturnExpr()
            newReturnExpr = [operator, returnExpr]
            return Block(statements, newReturnExpr)
        elif isinstance(iterable, PipIterable):
            if isinstance(iterable, Range) and iterable.getUpper() is None:
                self.err.warn("Can't fold infinite Range")
                return nil
            elif len(iterable) == 0:
                return ptypes.toPipType(operator.default)
            else:
                iterable = list(iterable)
                if operator.associativity == "L":
                    # Left fold for left-associative operators
                    foldValue = iterable[0]
                    for val in iterable[1:]:
                        foldValue = self.evaluate([normalOp, foldValue, val])
                elif operator.associativity == "R":
                    # Right fold for right-associative operators
                    foldValue = iterable[-1]
                    for val in iterable[-2::-1]:
                        foldValue = self.evaluate([normalOp, val, foldValue])
                elif operator.associativity == "C":
                    # Chaining fold for chaining operators
                    if len(iterable) == 1:
                        foldValue = SCALAR_ONE
                    else:
                        chainExpr = [ops.chain, iterable[0]]
                        for val in iterable[1:]:
                            chainExpr.extend((normalOp, val))
                        foldValue = self.evaluate(chainExpr)
                else:
                    self.err.die("Implementation error: unknown "
                                 f"associativity {operator.associativity} "
                                 "in FOLDMETA")
                return foldValue
        elif iterable is nil:
            return nil
        else:
            self.err.warn(f"Can't fold {type(iterable)}")
            return nil

    def MAPMETA(self, operator, iterable):
        iterable = self.getRval(iterable)
        normalOp = operator.copy()
        normalOp.map = False
        if isinstance(iterable, Block):
            # Create a lambda expression instead
            statements = iterable.getStatements()
            returnExpr = iterable.getReturnExpr()
            newReturnExpr = [operator, returnExpr]
            return Block(statements, newReturnExpr)
        elif isinstance(iterable, PipIterable):
            return List(self.evaluate([normalOp, item]) for item in iterable)
        elif iterable is nil:
            return nil
        else:
            self.err.warn(f"Can't map operator over {type(iterable)}")
            return nil

    def SCANMETA(self, operator, iterable):
        iterable = self.getRval(iterable)
        normalOp = operator.copy()
        normalOp.scan = False
        if isinstance(iterable, Block):
            # Create a lambda expression instead
            statements = iterable.getStatements()
            returnExpr = iterable.getReturnExpr()
            newReturnExpr = [operator, returnExpr]
            return Block(statements, newReturnExpr)
        elif isinstance(iterable, PipIterable):
            if isinstance(iterable, Range) and iterable.getUpper() is None:
                self.err.warn("Can't scan infinite Range")
                return nil
            elif len(iterable) == 0:
                # Scanning an empty iterable returns empty List
                return List([])
            else:
                iterable = list(iterable)
                if operator.associativity == "L":
                    # Left scan for left-associative operators
                    scanValue = iterable[0]
                    scanResults = [scanValue]
                    for val in iterable[1:]:
                        scanValue = self.evaluate([normalOp, scanValue, val])
                        scanResults.append(scanValue)
                elif operator.associativity == "R":
                    # Right scan for right-associative operators
                    scanValue = iterable[-1]
                    scanResults = [scanValue]
                    for val in iterable[-2::-1]:
                        scanValue = self.evaluate([normalOp, val, scanValue])
                        scanResults.append(scanValue)
                    scanResults.reverse()
                elif operator.associativity == "C":
                    # Chaining scan for chaining operators
                    chainExpr = [ops.chain, iterable[0]]
                    scanResults = [SCALAR_ONE]
                    for val in iterable[1:]:
                        chainExpr.extend((normalOp, val))
                        scanResults.append(self.evaluate(chainExpr))
                        # TODO: a better implementation that doesn't
                        # evaluate progressively longer chains
                else:
                    self.err.die("Implementation error: unknown "
                                 f"associativity {operator.associativity} "
                                 "in SCANMETA")
                return List(scanResults)
        elif iterable is nil:
            return nil
        else:
            self.err.warn(f"Can't scan {type(iterable)}")
            return nil

    ###############################
    ### Pip built-in operators  ###
    ###############################

    def ABS(self, rhs):
        if isinstance(rhs, Scalar):
            result = abs(rhs.toNumber())
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtype for ABS:", type(rhs))
            return nil

    def ABSOLUTEDIFF(self, lhs, rhs):
        if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
            result = abs(lhs.toNumber() - rhs.toNumber())
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtypes for ABSOLUTEDIFF:",
                          type(lhs), "and", type(rhs))
            return nil

    def ADD(self, lhs, rhs):
        if isinstance(lhs, Range) and isinstance(rhs, Scalar):
            lhs, rhs = rhs, lhs
        if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
            result = lhs.toNumber() + rhs.toNumber()
            return Scalar(result)
        elif isinstance(lhs, Scalar) and isinstance(rhs, Range):
            if lhs.toNumber() == int(lhs):
                lower = rhs.getLower() or 0
                upper = rhs.getUpper()
                lower += int(lhs)
                if upper is not None:
                    upper += int(lhs)
                else:
                    upper = nil
                return Range(lower, upper)
            else:
                return List(self.ADD(lhs, item) for item in rhs)
        elif isinstance(lhs, Pattern) and isinstance(rhs, Pattern):
            # + with two Patterns returns a new Pattern that matches one,
            # then the other
            return lhs.wrap().concat(rhs.wrap())
        else:
            self.err.warn("Unimplemented argtypes for ADD:",
                          type(lhs), "and", type(rhs))
            return nil

    def AND(self, lhs, rhs):
        # Short-circuiting AND operator
        result = self.getRval(lhs)
        if result:
            # The lhs was true, so we need to check the rhs
            result = self.getRval(rhs)
        return result

    def APPENDELEM(self, lhs, rhs):
        if isinstance(lhs, (Scalar, Pattern, Nil)):
            lhs = List([lhs])
        if isinstance(lhs, (List, Range)):
            return List(list(lhs) + [rhs])
        else:
            self.err.warn("Unimplemented argtypes for APPENDELEM:",
                          type(lhs), "and", type(rhs))
            return nil

    def APPENDLIST(self, lhs, rhs):
        if isinstance(lhs, (Scalar, Pattern, Nil)):
            lhs = List([lhs])
        if isinstance(rhs, (Scalar, Pattern, Nil)):
            rhs = List([rhs])
        if isinstance(lhs, (List, Range)) and isinstance(rhs, (List, Range)):
            return List(list(lhs) + list(rhs))
        else:
            self.err.warn("Unimplemented argtypes for APPENDLIST:",
                          type(lhs), "and", type(rhs))
            return nil

    def ARCTAN(self, lhs, rhs=None):
        if rhs is None:
            if isinstance(lhs, Scalar):
                return Scalar(math.atan(lhs.toNumber()))
            else:
                self.err.warn("Unimplemented argtype for ARCTAN:", type(rhs))
                return nil
        else:
            if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
                return Scalar(math.atan2(lhs.toNumber(), rhs.toNumber()))
            else:
                self.err.warn("Unimplemented argtypes for ARCTAN:",
                              type(lhs), "and", type(rhs))
                return nil

    def ASC(self, rhs):
        if isinstance(rhs, Scalar):
            if len(rhs) > 0:
                result = ord(str(rhs)[0])
                return Scalar(result)
            else:
                self.err.warn("Cannot take ASC of empty string")
                return nil
        elif isinstance(rhs, Pattern):
            # Given a Pattern, A toggles the ASCII-only flag
            return Pattern(rhs, re.ASCII)
        else:
            self.err.warn("Unimplemented argtype for ASC:", type(rhs))
            return nil

    def ASSIGN(self, lhs, rhs):
        if not isinstance(lhs, Lval):
            self.err.warn("Attempting to assign to non-lvalue", lhs)
            return rhs  # Gives correct result of 7 for 4+:3
        else:
            # If the rhs is an lval, get its rval
            if isinstance(rhs, Lval):
                rhs = self.getRval(rhs)
            self.assign(lhs, rhs)
            return lhs

    def AT(self, lhs, rhs=None):
        if isinstance(rhs, Lval):
            rhs = self.getRval(rhs)
        
        if isinstance(rhs, Scalar):
            index = int(rhs)
        elif isinstance(rhs, Range):
            index = rhs.toSlice()
        elif isinstance(rhs, (List, Pattern)):
            index = rhs
        elif rhs is None:
            index = 0
        else:
            self.err.warn("Cannot use", type(rhs), "as index")
            return nil

        if isinstance(lhs, Lval):
            if isinstance(index, (int, slice)):
                # Indexing using a Scalar or a Range returns an Lval
                if isinstance(lhs.base, List):
                    # The lhs is a list of lvalues; index into that list
                    try:
                        result = lhs.base[index]
                    except IndexError:
                        self.err.warn(f"Invalid index into {lhs!r}: {index}")
                        return nil
                    if isinstance(result, List):
                        return Lval(result)
                    elif isinstance(result, Lval):
                        return result
                    else:
                        self.err.die("Implementation error: reached else "
                                     "branch of Lval<List> AT int/slice, "
                                     "got", repr(result))

                else:
                    # The lhs is a single lvalue; attach the index to it
                    return Lval(lhs, index)
            elif isinstance(index, (List, Pattern)):
                # Using a List to index or doing a regex search can only
                # give you an rval
                lhs = self.getRval(lhs)
        
        if isinstance(rhs, Pattern) and isinstance(lhs, Scalar):
            matches = rhs.asRegex().finditer(str(lhs))
            result = List()
            for matchObj in matches:
                groups = self.assignRegexVars(matchObj)
                result.append(groups[0])
            return result
        elif isinstance(rhs, Pattern) and isinstance(lhs, (List, Range)):
            return List(self.AT(item, rhs) for item in lhs)
        elif isinstance(rhs, List) and isinstance(lhs, PipIterable):
            return List(self.AT(lhs, item) for item in rhs)
        elif isinstance(lhs, PipIterable):
            if lhs.isEmpty():
                self.err.warn("Indexing into empty iterable")
                return nil
            try:
                return lhs[index]
            except IndexError:
                self.err.warn(f"Invalid index into {lhs!r}: {index}")
                return nil
        else:
            self.err.warn("Cannot index into", type(lhs))
            return nil

    def BINARYLOG(self, number):
        """Take the base-2 logarithm of number."""
        if isinstance(number, Scalar):
            if number.toNumber() > 0:
                result = math.log2(number.toNumber())
                return Scalar(result)
            else:
                self.err.warn("Can't take logarithm of nonpositive number",
                              number)
                return nil
        else:
            self.err.warn("Unimplemented argtype for BINARYLOG:",
                          type(number))
            return nil
    
    def BITLENGTH(self, lhs):
        if isinstance(lhs, Scalar):
            return Scalar(int(lhs).bit_length())
        else:
            self.err.warn("Cannot get bit-length of", type(lhs))
            return nil
            
    def BITWISEAND(self, lhs, rhs):
        if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
            result = int(lhs) & int(rhs)
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtypes for BITWISEAND:",
                          type(lhs), "and", type(rhs))
            return nil

    def BITWISENOT(self, rhs):
        if isinstance(rhs, Scalar):
            result = ~int(rhs)
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtype for BITWISENOT:", type(rhs))
            return nil

    def BITWISEOR(self, lhs, rhs):
        if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
            result = int(lhs) | int(rhs)
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtypes for BITWISEOR:",
                          type(lhs), "and", type(rhs))
            return nil

    def BITWISEXOR(self, lhs, rhs):
        if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
            result = int(lhs) ^ int(rhs)
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtypes for BITWISEXOR:",
                          type(lhs), "and", type(rhs))
            return nil

    def BLOCK(self, statements):
        if len(statements) > 0 and parsing.isExpr(statements[-1]):
            # The last expression is the return value of the function
            returnExpr = statements[-1]
            statements = statements[:-1]
        else:
            returnExpr = None
        return Block(statements, returnExpr)
    
    def CARTESIANPRODUCT(self, list1, list2=None):
        if list2 is None:
            if isinstance(list1, PipIterable):
                lists = list1
            else:
                self.err.warn("Unimplemented argtype for CARTESIANPRODUCT:",
                              type(list1))
                return nil
        else:
            lists = [list1, list2]
        noniterables = [item for item in lists
                        if isinstance(item, (Nil, Block, Pattern))]
        if noniterables:
            # There are some of the "lists" that are not iterable
            # TBD: maybe this can find a non-error meaning?
            self.err.warn("Trying to take CP of non-iterable value(s):",
                          noniterables)
            return nil
        else:
            return List(List(tuple) for tuple in itertools.product(*lists))

    def CAT(self, lhs, rhs):
        if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
            result = str(lhs) + str(rhs)
            return Scalar(result)
        elif (isinstance(lhs, (Scalar, Pattern))
              and isinstance(rhs, (Scalar, Pattern))):
            return Pattern(lhs).concat(Pattern(rhs))
        else:
            self.err.warn("Unimplemented argtypes for CAT:",
                          type(lhs), "and", type(rhs))
            return nil

    def CEIL(self, lhs, rhs=None):
        """Round lhs to the nearest higher multiple of rhs."""
        if rhs is None:
            precision = 1
        elif isinstance(rhs, Scalar):
            precision = abs(rhs.toNumber())
        else:
            precision = None
        if isinstance(lhs, Scalar) and precision is not None:
            if precision == 0:
                return lhs
            elif precision == 1:
                lhs = lhs.toNumber()
                return Scalar(math.ceil(lhs))
            else:
                lhs = lhs.toNumber()
                return Scalar(math.ceil(lhs / precision) * precision)
        else:
            if rhs is None:
                self.err.warn("Unimplemented argtype for CEIL:",
                              type(lhs))
            else:
                self.err.warn("Unimplemented argtypes for CEIL:",
                              type(lhs), "and", type(rhs))
            return nil

    def CHAIN(self, *chain):
        # The args here alternate between rvals and comparison operators
        if len(chain) % 2 == 0:
            # An even chain length signals a malformed chain
            self.err.die("Implementation error: badly formed "
                         f"comparison chain {chain}")
        result = True
        i = 1      # i is the index of the next comparison operator in chain
        while result and i < len(chain):
            # Construct an ersatz parse tree to evaluate just this portion
            # of the chain: comparison operator, left-hand side, right-hand
            # side
            compTree = [chain[i], chain[i-1], chain[i+1]]
            # The result so far was true if we're still in the loop, so the
            # following is sufficient for correct evaluation:
            result = self.evaluate(compTree)
            # Skip to the next operator
            i += 2
        return result

    def CHOP(self, iterable, rhs=None):
        if rhs is None:
            rhs = SCALAR_TWO
        if isinstance(rhs, (List, Range)):
            # Chop by each rhs and return a list of iterables
            return List(self.CHOP(iterable, count) for count in rhs)
        if isinstance(iterable, PipIterable) and isinstance(rhs, Scalar):
            result = List()
            chunks = rhs.toNumber()
            if chunks > 0:
                index = 0
                jump = len(iterable) / chunks
                while index < len(iterable):
                    endIndex = min(index + jump, len(iterable))
                    chunk = iterable[math.floor(index):math.floor(endIndex)]
                    result.append(chunk)
                    index += jump
                return result
            elif chunks < 0:
                # With a negative chunk number, chop from right to left
                index = len(iterable)
                jump = len(iterable) / chunks
                while index > 0:
                    startIndex = max(index + jump, 0)
                    chunk = iterable[math.ceil(startIndex):math.ceil(index)]
                    result.append(chunk)
                    index += jump
                return result
            else:
                self.err.warn("Cannot CHOP into 0 slices")
                return nil
        else:
            self.err.warn("Unimplemented argtypes for CHOP:",
                          type(iterable), "and", type(rhs))
            return nil

    def CHR(self, rhs):
        if isinstance(rhs, Scalar):
            result = chr(int(rhs))
            return Scalar(result)
        elif isinstance(rhs, Pattern):
            # Given a Pattern, C wraps the regex in a capturing group
            return rhs.group()
        else:
            self.err.warn("Unimplemented argtype for CHR:", type(rhs))
            return nil

    def COMBINATIONS(self, iterable, num):
        """Return List of all ways to choose num items from iterable."""
        if isinstance(iterable, PipIterable) and isinstance(num, Scalar):
            result = itertools.combinations(iterable, int(num))
            if isinstance(iterable, Scalar):
                return List(self.JOIN(comb) for comb in result)
            else:
                return List(List(comb) for comb in result)
        else:
            self.err.warn("Unimplemented argtypes for COMBINATIONS:",
                          type(iterable), "and", type(num))
            return nil
    
    def COORDINATEGRID(self, rows, cols=None):
        if cols is None:
            cols = rows
        if isinstance(rows, Scalar) and isinstance(cols, Scalar):
            rows = range(int(rows))
            cols = range(int(cols))
            return List(List(List([Scalar(row), Scalar(col)])
                             for col in cols)
                        for row in rows)
        else:
            self.err.warn("Unimplemented argtypes for COORDINATEGRID:",
                          type(rows), "and", type(cols))
            return nil

    def COSEC(self, rhs):
        if isinstance(rhs, Scalar):
            return Scalar(1/math.sin(rhs.toNumber()))
        else:
            self.err.warn("Unimplemented argtype for COSEC:", type(rhs))
            return nil

    def COSINE(self, rhs):
        if isinstance(rhs, Scalar):
            return Scalar(math.cos(rhs.toNumber()))
        else:
            self.err.warn("Unimplemented argtype for COSINE:", type(rhs))
            return nil

    def COTAN(self, rhs):
        if isinstance(rhs, Scalar):
            return Scalar(1/math.tan(rhs.toNumber()))
        else:
            self.err.warn("Unimplemented argtype for COTAN:", type(rhs))
            return nil
        
    def DEC(self, rhs):
        minus = ops.opsByArity[2]["-"]
        result = self.evaluate([minus, self.getRval(rhs), SCALAR_ONE])
        if isinstance(rhs, Lval):
            # Subtract one and assign back to rhs
            self.assign(rhs, result)
            return rhs
        else:
            self.err.warn("Decrementing non-lvalue", rhs)
            # The expression still evaluates to the value minus one, though
            return result

    def DECIMALLOG(self, number):
        """Take the base-10 logarithm of number."""
        if isinstance(number, Scalar):
            if number.toNumber() > 0:
                result = math.log10(number.toNumber())
                return Scalar(result)
            else:
                self.err.warn("Can't take logarithm of nonpositive number",
                              number)
                return nil
        else:
            self.err.warn("Unimplemented argtype for DECIMALLOG:",
                          type(number))
            return nil

    def DELETECHARS(self, string, chars):
        """Delete characters from string."""
        if isinstance(string, Scalar) and isinstance(chars, Scalar):
            result = str(string).translate({ord(c):None for c in str(chars)})
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtypes for DELETECHARS:",
                          type(string), "and", type(chars))
            # Nothing to delete, so return original value
            return string

    def DEGREES(self, rhs):
        """Convert from radians to degrees."""
        if isinstance(rhs, Scalar):
            result = rhs.toNumber() / math.pi * 180
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtype for DEGREES:", type(rhs))
            return nil

    def DEQUEUE(self, iterable):
        iterVal = self.getRval(iterable)
        if isinstance(iterVal, List):
            if len(iterVal) > 0:
                item = iterVal[-1]
                iterVal = iterVal[:-1]
            else:
                self.err.warn("Dequeuing from empty list")
                return nil
        elif isinstance(iterVal, Range):
            try:
                if len(iterVal) > 0:
                    iterVal = Range(iterVal.getLower(),
                                    iterVal.getUpper() - 1)
                    item = Scalar(iterVal.getUpper())
                else:
                    self.err.warn("Dequeuing from empty range")
                    return nil
            except ValueError:
                # Infinite range raises this when you try to take the len()
                self.err.warn("Cannot dequeue from infinite range")
                return nil
        elif isinstance(iterVal, Scalar):
            if len(iterVal) > 0:
                item = iterVal[-1]
                iterVal = iterVal[:-1]
            else:
                self.err.warn("Dequeuing from empty scalar")
                return nil
        else:
            self.err.warn("Unimplemented argtype for DEQUEUE:", type(iterVal))
            return nil
        if isinstance(iterable, Lval):
            if not isinstance(iterable.base, List):
                self.assign(iterable, iterVal)
        else:
            self.err.warn("Dequeuing from non-lvalue", iterable)
        return item

    def DESCENDINGKEYED(self, keyFunction, iterable):
        """Sort descending by value of key function applied to each item.

        The key function is expected to return a number. It can also
        work if it returns a List, as long as it always returns a
        List with the same structure. Finite Ranges also work.
        """
        if not isinstance(keyFunction, Block) and isinstance(iterable, Block):
            keyFunction, iterable = iterable, keyFunction
        if (isinstance(keyFunction, Block)
                and isinstance(iterable, PipIterable)):
            def pyKey(enumitem):
                index, item = enumitem
                keyValue = self.functionCall(keyFunction,
                                             [item, Scalar(index)])
                if isinstance(keyValue, (Pattern, Block, Nil)):
                    self.err.warn("Replacing key value of type "
                                  f"{type(keyValue)} with 0 in "
                                  "DESCENDINGKEYED")
                    return 0
                elif (isinstance(keyValue, Range)
                      and keyValue.getUpper() is None):
                    self.err.warn("Replacing infinite Range key value "
                                  "with [] in DESCENDINGKEYED")
                    return []
                else:
                    # Convert Scalars to numbers, (nested) Lists to
                    # (nested) lists of numbers, Ranges to lists of numbers
                    return keyValue.toNumber()
            if iterable.isFinite():
                try:
                    return List(item
                                for index, item in sorted(enumerate(iterable),
                                                          key=pyKey,
                                                          reverse=True))
                except TypeError:
                    self.err.warn("DESCENDINGKEYED cannot compare numbers "
                                  "to lists")
                    return nil
            else:
                self.err.warn("DESCENDINGKEYED cannot sort infinite Range")
                return nil
        else:
            self.err.warn("Unimplemented argtypes for DESCENDINGKEYED:",
                          type(keyFunction), "and", type(iterable))
            return nil

    def DESCENDINGNUM(self, iterable):
        if isinstance(iterable, PipIterable):
            def pyKey(item):
                if isinstance(item, (Pattern, Block, Nil)):
                    self.err.warn(f"Items of type {type(item)} "
                                  "are sorted as if they were 0 "
                                  "in DESCENDINGNUM")
                    return 0
                elif isinstance(item, Range) and item.getUpper() is None:
                    self.err.warn("Infinite Ranges are sorted as if "
                                  "they were [] in DESCENDINGNUM")
                    return []
                else:
                    # Treat Scalars as numbers, (nested) Lists as
                    # (nested) lists of numbers, Ranges as lists of numbers
                    return item.toNumber()
            try:
                return List(sorted(iterable, key=pyKey, reverse=True))
            except TypeError:
                self.err.warn("DESCENDINGNUM cannot compare numbers to lists")
                return nil
        else:
            self.err.warn("Unimplemented argtype for DESCENDINGNUM:",
                          type(iterable))
            return nil

    def DESCENDINGSTRING(self, iterable):
        if isinstance(iterable, Scalar):
            return Scalar("".join(sorted(str(iterable),
                                         reverse=True)))
        elif isinstance(iterable, PipIterable):
            # This is going to get a bit wonky when sorting lists of lists,
            # but not sure it's worth the effort to fix
            return List(sorted(iterable,
                               key=str,
                               reverse=True))
        else:
            self.err.warn("Unimplemented argtype for DESCENDINGSTRING:",
                          type(iterable))
            return nil
    
    def DIV(self, lhs, rhs):
        if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
            try:
                result = lhs.toNumber() / rhs.toNumber()
                return Scalar(result)
            except ZeroDivisionError:
                self.err.warn("Dividing by zero")
                return nil
        else:
            self.err.warn("Unimplemented argtypes for DIV:",
                          type(lhs), "and", type(rhs))
            return nil

    def DOT(self, rhs):
        if isinstance(rhs, Pattern):
            # Given a Pattern, . makes regex . match newlines
            return Pattern(rhs, re.DOTALL)
        else:
            # For Scalars etc., pass through unchanged
            # TBD: is unary . useful for something for those types?
            return rhs

    def DOUBLE(self, rhs):
        if isinstance(rhs, Scalar):
            return Scalar(rhs.toNumber() * 2)
        else:
            self.err.warn("Unimplemented argtype for DOUBLE:",
                          type(rhs))
            return nil

    def ENUMERATE(self, iterable):
        if isinstance(iterable, PipIterable):
            if isinstance(iterable, Range) and iterable.getUpper() is None:
                self.err.warn("Cannot enumerate infinite range")
                return nil
            else:
                return List(List((Scalar(index), item))
                            for index, item in enumerate(iterable))
        else:
            self.err.warn("Unimplemented argtype for ENUMERATE:",
                          type(iterable))
            return nil

    def EVAL(self, code, argList=None):
        if isinstance(argList, Block):
            # The arguments are reversible to enable things like lV:f
            code, argList = argList, code
        if isinstance(code, Scalar):
            # Scan, parse, and convert to Block first
            try:
                tkns = scanning.scan(str(code) + "\n")
            except FatalError as err:
                self.err.die(f"Scanning error while evaluating {code!r}:",
                             err)
            try:
                tree = parsing.parse(tkns)
            except FatalError as err:
                self.err.die(f"Parsing error while evaluating {code!r}:",
                             err)
            code = self.BLOCK(tree)
        if isinstance(code, Block) and argList is not None:
            return self.functionCall(code, argList)
        elif isinstance(code, Block) and argList is None:
            # Not a function call--just run the block at current scope
            for statement in code.getStatements():
                self.executeStatement(statement)
            return self.evaluate(code.getReturnExpr())
        else:
            if argList is None:
                self.err.warn("Unimplemented argtype for EVAL:",
                              type(code))
            else:
                self.err.warn("Unimplemented argtypes for EVAL:",
                              type(code), "and", type(argList))
            return nil

    def EXPONENTIAL(self, number):
        """Take e to the power of number."""
        if isinstance(number, Scalar):
            result = math.exp(number.toNumber())
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtype for EXPONENTIAL:",
                          type(number))
            return nil

    def FILTER(self, function, iterable=None):
        """Filter iterable: keep items where function returns truthy."""
        if iterable is None:
            # The unary version keeps items that are truthy
            function, iterable = iterable, function
            if isinstance(iterable, PipIterable):
                return List(item for item in iterable if item)
            else:
                self.err.warn("Unimplemented argtype for FILTER:",
                              type(iterable))
                return nil
        if isinstance(iterable, Block) and isinstance(function, PipIterable):
            # The arguments are reversible to enable things like lFI:f
            function, iterable = iterable, function
        if isinstance(function, Block) and isinstance(iterable, PipIterable):
            return List(item
                        for index, item in enumerate(iterable)
                        if self.functionCall(function,
                                             [item, Scalar(index)]))
        else:
            self.err.warn("Unimplemented argtypes for FILTER:",
                          type(function), "and", type(iterable))
            return nil

    def FILTERENUMERATE(self, function, iterable):
        """Filter iterable: keep items where function returns truthy.

        The function is passed two arguments: the index of the item
        in the iterable, and the item itself.
        """
        if isinstance(iterable, Block) and isinstance(function, PipIterable):
            # The arguments are reversible to enable things like lFE:f
            function, iterable = iterable, function
        if isinstance(function, Block) and isinstance(iterable, PipIterable):
            return List(item
                        for index, item in enumerate(iterable)
                        if self.functionCall(function,
                                             [Scalar(index), item]))
        else:
            self.err.warn("Unimplemented argtypes for FILTERENUMERATE:",
                          type(function), "and", type(iterable))
            return nil

    def FILTERFLATTEN(self, function, iterable=None):
        """Same as FILTER, but flatten the result by one level afterwards."""
        return self.FLATTEN(self.FILTER(function, iterable))

    def FILTERJOIN(self, function, iterable=None):
        """Same as FILTER, but join the result into a string afterwards."""
        return self.JOIN(self.FILTER(function, iterable))

    def FILTERINDEXES(self, function, iterable=None):
        """Filter iterable: keep indexes where function returns truthy.

        The function is passed two arguments: the index of the item
        in the iterable, and the item itself.
        """
        if iterable is None:
            # The unary version keeps indexes whose items are truthy
            function, iterable = iterable, function
            if isinstance(iterable, PipIterable):
                return List(Scalar(index)
                            for index, item in enumerate(iterable)
                            if item)
            else:
                self.err.warn("Unimplemented argtype for FILTERINDEXES:",
                              type(iterable))
                return nil
        if isinstance(iterable, Block) and isinstance(function, PipIterable):
            # The arguments are reversible to enable things like lFX:f
            function, iterable = iterable, function
        if isinstance(function, Block) and isinstance(iterable, PipIterable):
            return List(Scalar(index)
                        for index, item in enumerate(iterable)
                        if self.functionCall(function,
                                             [Scalar(index), item]))
        else:
            self.err.warn("Unimplemented argtypes for FILTERINDEXES:",
                          type(function), "and", type(iterable))
            return nil

    def FILTERNOT(self, function, iterable=None):
        """Filter iterable: keep items where function returns falsey."""
        if iterable is None:
            # The unary version keeps items that are falsey
            function, iterable = iterable, function
            if isinstance(iterable, PipIterable):
                return List(item for item in iterable if not item)
            else:
                self.err.warn("Unimplemented argtype for FILTERNOT:",
                              type(iterable))
                return nil
        if isinstance(iterable, Block) and isinstance(function, PipIterable):
            # The arguments are reversible to enable things like lFN:f
            function, iterable = iterable, function
        if isinstance(function, Block) and isinstance(iterable, PipIterable):
            return List(item
                        for index, item in enumerate(iterable)
                        if not self.functionCall(function,
                                                 [item, Scalar(index)]))
        else:
            self.err.warn("Unimplemented argtypes for FILTERNOT:",
                          type(function), "and", type(iterable))
            return nil

    def FILTERUNPACK(self, function, iterable=None):
        """Filter iterable: keep items where function returns truthy.

        The function is passed one argument for each element of each
        item in the iterable.
        """
        if iterable is None:
            # The unary version keeps items whose first element is truthy
            function, iterable = iterable, function
            if isinstance(iterable, PipIterable):
                result = List()
                for item in iterable:
                    if isinstance(item, PipIterable):
                        if not item.isEmpty() and item[0]:
                            result.append(item)
                    else:
                        self.err.warn(f"Cannot unpack {type(item)} value in "
                                      "FILTERUNPACK")
                return result
            else:
                self.err.warn("Unimplemented argtype for FILTERUNPACK:",
                              type(iterable))
                return nil
        if isinstance(iterable, Block) and isinstance(function, PipIterable):
            # The arguments are reversible to enable things like lFU:f
            function, iterable = iterable, function
        if isinstance(function, Block) and isinstance(iterable, PipIterable):
            result = List()
            for item in iterable:
                if isinstance(item, PipIterable):
                    if item.isFinite():
                        arglist = list(item)
                    else:
                        # The item is an infinite Range
                        self.err.warn("Cannot unpack infinite Range in "
                                      "FILTERUNPACK")
                        arglist = None
                else:
                    self.err.warn(f"Cannot unpack {type(item)} value in "
                                  "FILTERUNPACK")
                    arglist = None
                if arglist is not None:
                    if self.functionCall(function, arglist):
                        result.append(item)
            return result
        else:
            self.err.warn("Unimplemented argtypes for FILTERUNPACK:",
                          type(function), "and", type(iterable))
            return nil

    def FIND(self, iterable, item):
        if (isinstance(item, (List, Range))
                and isinstance(iterable, (Scalar, Range))):
            if item.isFinite():
                return List(self.FIND(iterable, subitem)
                            for subitem in item)
            else:
                self.err.warn("Cannot find indices of all items in "
                              f"infinite Range {item}")
                return nil
        elif isinstance(item, Pattern) and isinstance(iterable, Scalar):
            matchObj = item.asRegex().search(str(iterable))
            if matchObj:
                self.assignRegexVars(matchObj)
                return Scalar(matchObj.start())
            else:
                return nil
        elif (isinstance(item, Scalar) and isinstance(iterable, PipIterable)
              or isinstance(iterable, List)):
            try:
                return Scalar(iterable.index(item))
            except ValueError:
                # Item not found
                return nil
        else:
            self.err.warn("Unimplemented argtypes for FIND:",
                          type(iterable), "and", type(item))
            return nil

    def FINDALL(self, iterable, item):
        if (isinstance(item, (List, Range))
                and isinstance(iterable, (Scalar, Range))):
            if item.isFinite():
                return List(self.FINDALL(iterable, subitem)
                            for subitem in item)
            else:
                self.err.warn("Cannot find all indices of all items in "
                              f"infinite Range {item}")
                return nil
        elif isinstance(item, Pattern) and isinstance(iterable, Scalar):
            # Return indices of all regex matches in Scalar
            matches = item.asRegex().finditer(str(iterable))
            result = List()
            for matchObj in matches:
                self.assignRegexVars(matchObj)
                result.append(Scalar(matchObj.start()))
            return result
        elif (isinstance(item, Scalar) and isinstance(iterable, PipIterable)
              or isinstance(iterable, List)):
            # Return indices of all occurrences of item in iterable
            result = List()
            index = -1
            try:
                while True:
                    # Find the next index of item in iterable
                    index = iterable.index(item, index + 1)
                    result.append(Scalar(index))
            except ValueError:
                # Found all indices
                pass
            return result
        else:
            self.err.warn("Unimplemented argtypes for FINDALL:",
                          type(iterable), "and", type(item))
            return nil

    def FIRSTMATCH(self, string, regex):
        if isinstance(string, Pattern) and isinstance(regex, Scalar):
            regex, string = string, regex
        elif isinstance(regex, Scalar):
            regex = self.REGEX(regex)
        if isinstance(string, Scalar) and isinstance(regex, Pattern):
            matchObj = regex.asRegex().search(str(string))
            if matchObj:
                self.assignRegexVars(matchObj)
                return Scalar(matchObj.group())
            else:
                return nil
        else:
            self.err.warn("Unimplemented argtypes for FIRSTMATCH:",
                          type(string), "and", type(regex))
            return nil

    def FLATTEN(self, iterable):
        if isinstance(iterable, (List, Range)):
            if iterable.isFinite():
                result = List()
                for item in iterable:
                    if isinstance(item, (List, Range)):
                        if item.isFinite():
                            result.extend(item)
                        else:
                            self.err.warn("Cannot FLATTEN List",
                                          "containing infinite Range")
                            return nil
                    else:
                        result.append(item)
                return result
            else:
                self.err.warn("Cannot FLATTEN infinite Range")
                return nil
        else:
            return iterable

    def FLATTENALL(self, iterable):
        if isinstance(iterable, (List, Range)):
            if iterable.isFinite():
                result = List()
                for item in iterable:
                    if isinstance(item, List):
                        flatList = self.FLATTENALL(item)
                        if flatList is nil:
                            # A nested List caused an error condition;
                            # propagate it
                            return nil
                        else:
                            result.extend(self.FLATTENALL(item))
                    elif isinstance(item, Range):
                        if item.isFinite():
                            result.extend(item)
                        else:
                            self.err.warn("Cannot FLATTENALL List",
                                          "containing infinite Range")
                            return nil
                    else:
                        result.append(item)
                return result
            else:
                self.err.warn("Cannot FLATTENALL infinite Range")
                return nil
        else:
            return iterable

    def FLOOR(self, lhs, rhs=None):
        """Round lhs to the nearest lower multiple of rhs."""
        if rhs is None:
            precision = 1
        elif isinstance(rhs, Scalar):
            precision = abs(rhs.toNumber())
        else:
            precision = None
        if isinstance(lhs, Scalar) and precision is not None:
            if precision == 0:
                return lhs
            elif precision == 1:
                lhs = lhs.toNumber()
                return Scalar(math.floor(lhs))
            else:
                lhs = lhs.toNumber()
                return Scalar(math.floor(lhs / precision) * precision)
        else:
            if rhs is None:
                self.err.warn("Unimplemented argtype for FLOOR:",
                              type(lhs))
            else:
                self.err.warn("Unimplemented argtypes for FLOOR:",
                              type(lhs), "and", type(rhs))
            return nil

    def FROMBASE(self, number, base=None):
        """Convert a string in the specified base to a decimal integer."""
        if base is None:
            base = 2
        elif isinstance(base, Scalar):
            base = int(base)
        elif isinstance(base, int):
            pass
        else:
            self.err.warn("Unimplemented base type for FROMBASE:", type(base))
            return nil
        if base < 2 or base > len(BASE_CONVERSION_DIGITS):
            self.err.warn("Invalid base for FROMBASE:", base)
            return nil
        if isinstance(number, Scalar):
            number = str(number)
            if number.startswith("-"):
                sign = -1
                number = number[1:]
            else:
                sign = 1
            result = 0
            for digit in number:
                if digit in BASE_CONVERSION_DIGITS:
                    digitValue = BASE_CONVERSION_DIGITS.index(digit)
                    if digitValue >= base:
                        # This digit is larger than the max digit in the
                        # given base, but we can keep going with the
                        # conversion; just give a warning
                        if digit == str(digitValue):
                            digitRepresentation = digit
                        else:
                            digitRepresentation = f"{digit} = {digitValue}"
                        self.err.warn("Digit too large for base", base,
                                      "in FROMBASE:", digitRepresentation)
                    result = result * base + digitValue
                else:
                    self.err.warn("Unrecognized digit in FROMBASE:", digit)
                    return nil
            return Scalar(sign * result)
        else:
            self.err.warn("Unimplemented argtype for FROMBASE:",
                          type(number))
            return nil

    def FROMDIGITS(self, digits, base=None):
        "Convert a list of digits in the specified base to a decimal integer."
        if base is None:
            base = 2
        elif isinstance(base, Scalar):
            base = base.toNumber()
        elif isinstance(base, int):
            pass
        else:
            self.err.warn("Unimplemented base type for FROMDIGITS:",
                          type(base))
            return nil
        if isinstance(digits, PipIterable):
            result = 0
            for exponent, digit in enumerate(reversed(digits)):
                if isinstance(digit, Scalar):
                    digit = digit.toNumber()
                    result += digit * base ** exponent
                else:
                    self.err.warn("Digits in FROMDIGITS must each be "
                                  "Scalar, not", type(digit))
                    return nil
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtype for FROMDIGITS:",
                          type(digits))
            return nil

    def FROMHEX(self, number):
        """Convert a string in hexadecimal to a decimal integer."""
        return self.FROMBASE(number, 16)

    def FULLMATCH(self, string, regex):
        if isinstance(string, Pattern) and isinstance(regex, Scalar):
            regex, string = string, regex
        elif isinstance(regex, Scalar):
            regex = self.REGEX(regex)
        if isinstance(string, (List, Range)) and isinstance(regex, Pattern):
            return Scalar(all(self.FULLMATCH(regex, item)
                              for item in string))
        elif isinstance(string, Scalar) and isinstance(regex, Pattern):
            matchObj = regex.asRegex().fullmatch(str(string))
            if matchObj:
                self.assignRegexVars(matchObj)
                return Scalar("1")
            else:
                return Scalar("0")
        else:
            self.err.warn("Unimplemented argtypes for FULLMATCH:",
                          type(string), "and", type(regex))
            return Scalar("0")

    def GROUP(self, iterable, rhs=None):
        if rhs is None:
            rhs = SCALAR_TWO
        if isinstance(rhs, (List, Range)):
            # Group by each rhs and return a list of iterables
            return List(self.GROUP(iterable, jump) for jump in rhs)
        if isinstance(iterable, PipIterable) and isinstance(rhs, Scalar):
            result = List()
            jump = rhs.toNumber()
            if jump > 0:
                index = 0
                while index < len(iterable):
                    endIndex = min(index + jump, len(iterable))
                    chunk = iterable[math.floor(index):math.floor(endIndex)]
                    result.append(chunk)
                    index += jump
                return result
            elif jump < 0:
                # With a negative jump, group from right to left
                index = len(iterable)
                while index > 0:
                    startIndex = max(index + jump, 0)
                    chunk = iterable[math.ceil(startIndex):math.ceil(index)]
                    result.append(chunk)
                    index += jump
                return result
            else:
                self.err.warn("Cannot GROUP into slices of size 0")
                return nil
        else:
            self.err.warn("Unimplemented argtypes for GROUP:",
                          type(iterable), "and", type(rhs))
            return nil

    def HALVE(self, rhs):
        """Halve and round down."""
        if isinstance(rhs, Scalar):
            return Scalar(math.floor(rhs.toNumber() / 2))
        else:
            self.err.warn("Unimplemented argtype for HALVE:",
                          type(rhs))
            return nil

    def HALVEUP(self, rhs):
        """Halve and round up."""
        if isinstance(rhs, Scalar):
            return Scalar(math.ceil(rhs.toNumber() / 2))
        else:
            self.err.warn("Unimplemented argtype for HALVEUP:",
                          type(rhs))
            return nil

    def IDENTITYMATRIX(self, rhs):
        if isinstance(rhs, Scalar):
            result = List()
            for row in range(int(rhs)):
                subresult = [Scalar("1" if row == col else "0")
                             for col in range(int(rhs))]
                result.append(List(subresult))
            return result
        else:
            self.err.warn("Unimplemented argtype for IDENTITYMATRIX:",
                          type(rhs))
            return nil

    def IFTE(self, test, trueBranch, falseBranch):
        # Ternary if-then-else operator
        test = self.getRval(test)
        if test:
            return self.evaluate(trueBranch)
        else:
            return self.evaluate(falseBranch)

    def IN(self, needle, haystack):
        """Count occurrences of needle in haystack.

        Given a Pattern and a Scalar, count regex matches instead.
        """
        if isinstance(needle, (List, Range)) and isinstance(haystack, Scalar):
            # It doesn't make sense for a List or Range to be a
            # substring of a Scalar, so return a List of the counts
            # of all items of needle in haystack
            return List(self.IN(item, haystack) for item in needle)
        elif isinstance(needle, Pattern) and isinstance(haystack, Scalar):
            matches = needle.asRegex().finditer(str(haystack))
            count = 0
            for matchObj in matches:
                self.assignRegexVars(matchObj)
                count += 1
            return Scalar(count)
        elif needle is nil and isinstance(haystack, (Scalar, Range)):
            return nil
        elif isinstance(haystack, PipIterable):
            return Scalar(haystack.count(needle))
        else:
            # If it's not one of those types, it's automatically false
            return Scalar("0")

    def INC(self, rhs):
        plus = ops.opsByArity[2]["+"]
        result = self.evaluate([plus, self.getRval(rhs), SCALAR_ONE])
        if isinstance(rhs, Lval):
            # Add one and assign back to rhs
            self.assign(rhs, result)
            return rhs
        else:
            self.err.warn("Incrementing non-lvalue", rhs)
            # The expression still evaluates to the value plus one, though
            return result

    def INCLRANGE(self, lhs, rhs):
        """Like RANGE, but include upper bound."""
        if isinstance(lhs, (Scalar, Nil)) and isinstance(lhs, (Scalar, Nil)):
            if isinstance(rhs, Scalar):
                return Range(lhs, int(rhs) + 1)
            elif rhs is nil:
                return Range(lhs, rhs)
        else:
            self.err.warn("Unimplemented argtypes for INCLRANGE:",
                          type(lhs), "and", type(rhs))
            return nil

    def INCLRANGETO(self, rhs):
        """Like RANGETO, but start at 1 and include upper bound."""
        if rhs is nil:
            return Range(1, rhs)
        elif isinstance(rhs, Scalar):
            return Range(1, int(rhs) + 1)
        else:
            self.err.warn("Unimplemented argtype for INCLRANGETO:", type(rhs))
            return nil

    def INITIALCAPS(self, rhs):
        if isinstance(rhs, Scalar):
            return Scalar(str(rhs).capitalize())
        elif isinstance(rhs, (Range, Nil)):
            return rhs
        else:
            self.err.warn("Unimplemented argtype for INITIALCAPS:", type(rhs))
            return nil

    def INTDIV(self, lhs, rhs):
        if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
            try:
                return Scalar(lhs.toNumber() // rhs.toNumber())
            except ZeroDivisionError:
                self.err.warn("Dividing by zero")
                return nil
        else:
            self.err.warn("Unimplemented argtypes for INTDIV:",
                          type(lhs), "and", type(rhs))
            return nil

    def INVERT(self, rhs):
        if isinstance(rhs, Scalar):
            try:
                result = 1 / rhs.toNumber()
                return Scalar(result)
            except ZeroDivisionError:
                self.err.warn("Inverting zero")
                return nil
        else:
            self.err.warn("Unimplemented argtype for INVERT:", type(rhs))
            return nil

    def JOIN(self, iterable, sep=None):
        if isinstance(sep, (List, Range)):
            return List(self.JOIN(iterable, item) for item in sep)
        elif sep is not None and not isinstance(sep, (Scalar, Pattern)):
            self.err.warn("Can't join on", type(sep))
            return nil

        if isinstance(iterable, (PipIterable, list, tuple)):
            result = None
            for item in iterable:
                if isinstance(item, (List, Range)):
                    item = self.JOIN(item, sep)
                if result is None:
                    result = item
                else:
                    if sep is not None:
                        result = self.CAT(result, sep)
                    result = self.CAT(result, item)
            if result is None:
                return Scalar("")
            else:
                return result
        else:
            if sep is None:
                self.err.warn("Unimplemented argtype for JOIN:",
                              type(iterable))
            else:
                self.err.warn("Unimplemented argtypes for JOIN:",
                              type(iterable), "and", type(sep))
            return nil

    def JOINWRAP(self, iterable, sep):
        if isinstance(sep, (List, Range)):
            return List(self.JOINWRAP(iterable, item) for item in sep)
        elif not isinstance(sep, (Scalar, Pattern)):
            self.err.warn("Can't join/wrap with", type(sep))
            return nil

        if isinstance(iterable, PipIterable):
            result = sep
            for item in iterable:
                if isinstance(item, (List, Range)):
                    item = self.JOIN(item, sep)
                result = self.CAT(result, item)
                result = self.CAT(result, sep)
            return result
        else:
            self.err.warn("Unimplemented argtypes for JOINWRAP:",
                          type(iterable), "and", type(sep))
            return nil

    def KLEENESTAR(self, rhs):
        if isinstance(rhs, (Scalar, Range)):
            return self.REGEX(rhs).kleeneStar()
        elif isinstance(rhs, Pattern):
            return rhs.wrap().kleeneStar()
        else:
            self.err.warn("Unimplemented argtype for KLEENESTAR:", type(rhs))
            return nil

    def LEFTOF(self, lhs, rhs=None):
        if rhs is None:
            # The unary version gives all but the rightmost character
            rhs = Scalar(-1)
        if isinstance(rhs, Lval):
            rhs = self.getRval(rhs)
        if isinstance(lhs, Lval) and isinstance(rhs, Scalar):
            index = slice(None, int(rhs))
            return Lval(lhs, index)
        elif isinstance(rhs, (List, Range)):
            if isinstance(lhs, Lval):
                lhs = self.getRval(lhs)
            return List(self.LEFTOF(lhs, index) for index in rhs)
        elif isinstance(lhs, PipIterable) and isinstance(rhs, Scalar):
            # Use the lhs's __getitem__ with a slice argument
            try:
                return lhs[:int(rhs)]
            except IndexError:
                if not lhs.isFinite():
                    self.err.warn("Cannot slice from end of infinite Range")
                    return nil
                else:
                    # Other cases shouldn't cause an IndexError, so it's
                    # probably an implementation bug
                    raise
        else:
            self.err.warn("Unimplemented argtypes for LEFTOF:",
                          type(lhs), "and", type(rhs))
            return nil

    def LEN(self, rhs):
        if isinstance(rhs, PipIterable):
            try:
                result = len(rhs)
            except ValueError:
                # Infinite Range does not have a length
                self.err.warn("Cannot take LEN of an infinite Range")
                return nil
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtype for LEN:", type(rhs))
            return nil

    def LENEQUAL(self, lhs, rhs):
        if isinstance(lhs, PipIterable) and isinstance(rhs, PipIterable):
            if lhs.isFinite() and rhs.isFinite():
                result = len(lhs) == len(rhs)
            else:
                # One or both of the arguments is an infinite Range
                # Their lengths are equal iff they are both infinite
                result = not lhs.isFinite() and not rhs.isFinite()
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtypes for LENEQUAL:",
                          type(lhs), "and", type(rhs))
            return nil

    def LENGREATER(self, lhs, rhs):
        if isinstance(lhs, PipIterable) and isinstance(rhs, PipIterable):
            if lhs.isFinite() and rhs.isFinite():
                result = len(lhs) > len(rhs)
            else:
                # One or both of the arguments is an infinite Range
                # The lhs's length is greater iff it is infinite
                # and the rhs is not
                result = not lhs.isFinite() and rhs.isFinite()
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtypes for LENGREATER:",
                          type(lhs), "and", type(rhs))
            return nil

    def LENLESS(self, lhs, rhs):
        if isinstance(lhs, PipIterable) and isinstance(rhs, PipIterable):
            if lhs.isFinite() and rhs.isFinite():
                result = len(lhs) < len(rhs)
            else:
                # One or both of the arguments is an infinite Range
                # The lhs's length is less iff it is not an infinite
                # Range and the rhs is one
                result = lhs.isFinite() and not rhs.isFinite()
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtypes for LENLESS:",
                          type(lhs), "and", type(rhs))
            return nil

    def LIST(self, *items):
        "Wrap any number of items in a List."
        if items and all(isinstance(item, Lval) for item in items):
            # If every item in the List is an lvalue, the List is an
            # lvalue (useful for destructuring assignment)
            return Lval(List(items))
        else:
            # If the List is empty or contains at least one rvalue,
            # the List is an rvalue
            return List(self.getRval(item) for item in items)

    def LOWERCASE(self, rhs):
        if isinstance(rhs, Scalar):
            return Scalar(str(rhs).lower())
        elif isinstance(rhs, (Range, Nil)):
            return rhs
        else:
            self.err.warn("Unimplemented argtype for LOWERCASE:", type(rhs))
            return nil

    def LSTRIP(self, string, extra=None):
        if extra is nil:
            return string
        elif (not isinstance(extra, (PipIterable, Pattern))
              and extra is not None):
            self.err.warn("Unimplemented argtype for rhs of LSTRIP:",
                          type(extra))
            return nil
            
        if isinstance(string, (List, Range)):
            return List(self.LSTRIP(item, extra) for item in string)
        elif isinstance(string, Scalar):
            if isinstance(extra, (List, Range)):
                for item in extra:
                    string = self.LSTRIP(string, item)
                return string
            elif isinstance(extra, Scalar):
                return Scalar(str(string).lstrip(str(extra)))
            elif isinstance(extra, Pattern):
                # Python doesn't have a regex strip operation--we have to
                # roll our own
                patternStr = str(extra)
                if patternStr == "":
                    return string
                oldString = str(string)
                beginning = re.compile("^" + patternStr)
                newString = beginning.sub("", oldString)
                while oldString != newString:
                    oldString = newString
                    newString = beginning.sub("", oldString)
                return Scalar(newString)
            elif extra is None:
                return Scalar(str(string).lstrip())
        else:
            self.err.warn("Unimplemented argtype for lhs of LSTRIP:",
                          type(string))
            return nil

    def MAP(self, lhs, iterable):
        """Map function over the items of iterable."""
        if isinstance(iterable, Block) and isinstance(lhs, PipIterable):
            # The arguments are reversible to enable things like lM:f
            lhs, iterable = iterable, lhs
        if isinstance(iterable, PipIterable):
            if isinstance(lhs, Block):
                result = (self.functionCall(lhs, [item, Scalar(index)])
                          for index, item in enumerate(iterable))
            elif isinstance(lhs, List):
                # Might be a list of functions; map each of them
                result = (self.MAP(item, iterable) for item in lhs)
            else:
                # If lhs isn't a function, just replace each item in iterable
                # with it
                # TBD: different behavior for Patterns?
                result = (lhs for item in iterable)
            return List(result)
        else:
            self.err.warn("Unimplemented argtypes for MAP:",
                          type(lhs), "and", type(iterable))
            return nil

    def MAPCOORDS(self, lhs, rhs):
        """Map function over grid of coordinate pairs."""
        if isinstance(rhs, Scalar):
            # A single number means a square grid of that size
            rows = range(int(rhs))
            cols = range(int(rhs))
        elif isinstance(rhs, (List, Range)):
            # A List/Range of two numbers means a rectangular grid
            # If there are more than two numbers, we use the first two;
            # if there is only one number, we use it twice; but if there
            # are no numbers, that's a problem
            if rhs.isEmpty():
                self.err.warn("Empty List/Range argument to MAPCOORDS")
                return nil
            if isinstance(rhs[0], Scalar) and isinstance(rhs[1], Scalar):
                rows = range(int(rhs[0]))
                cols = range(int(rhs[1]))
            else:
                self.err.warn("Unsupported argtypes in List argument "
                              "to MAPCOORDS:", type(rhs[0]), "and",
                              type(rhs[1]))
                return nil
        else:
            self.err.warn("Unimplemented argtypes for MAPCOORDS:",
                          type(lhs), "and", type(rhs))
            return nil
        result = List()
        for row in rows:
            subresult = List()
            for col in cols:
                if isinstance(lhs, Block):
                    subresult.append(self.functionCall(lhs,
                                                       [Scalar(row),
                                                        Scalar(col)]))
                else:
                    # If lhs isn't a function, just return a grid of it
                    subresult.append(lhs)
            result.append(subresult)
        return result

    def MAPENUMERATE(self, function, iterable):
        """Map function over index/value pairs of items of the iterable."""
        if isinstance(iterable, Block) and isinstance(function, PipIterable):
            # The arguments are reversible to enable things like lME:f
            function, iterable = iterable, function
        if isinstance(function, Block) and isinstance(iterable, PipIterable):
            return self.MAPZIP(function, Range(len(iterable)), iterable)
        else:
            self.err.warn("Unimplemented argtypes for MAPENUMERATE:",
                          type(function), "and", type(iterable))
            return nil

    def MAPFLATTEN(self, lhs, iterable):
        """Same as MAP, but flatten the result by one level afterwards."""
        return self.FLATTEN(self.MAP(lhs, iterable))

    def MAPJOIN(self, lhs, iterable):
        """Same as MAP, but join the result into a string afterwards.

        a MJ b == J(a M b)
        """
        return self.JOIN(self.MAP(lhs, iterable))

    def MAPMAP(self, lhs, iterable):
        """Map function over the items of the items of iterable."""
        if isinstance(iterable, Block) and isinstance(lhs, PipIterable):
            # The arguments are reversible to enable things like lMM:f
            lhs, iterable = iterable, lhs
        if isinstance(iterable, PipIterable):
            if isinstance(lhs, Block):
                result = (List(self.functionCall(lhs, [subitem,
                                                       Scalar(outerIndex),
                                                       Scalar(innerIndex)])
                               for innerIndex, subitem in enumerate(item))
                          for outerIndex, item in enumerate(iterable))
            elif isinstance(lhs, List):
                # Might be a list of functions; map each of them
                result = (self.MAPMAP(item, iterable) for item in lhs)
            else:
                # If lhs isn't a function, just replace each subitem
                # in iterable with it
                # TBD: different behavior for Patterns?
                result = (List(lhs for subitem in item)
                          for item in iterable)
            return List(result)
        else:
            self.err.warn("Unimplemented argtypes for MAPMAP:",
                          type(function), "and", type(iterable))
            return nil

    def MAPPAIRS(self, function, iterable):
        """Map function over consecutive pairs of items of the iterable."""
        if isinstance(iterable, Block) and isinstance(function, PipIterable):
            # The arguments are reversible to enable things like lMP:f
            function, iterable = iterable, function
        if isinstance(function, Block) and isinstance(iterable, PipIterable):
            return self.MAPZIP(function, iterable, iterable[1:])
        else:
            self.err.warn("Unimplemented argtypes for MAPPAIRS:",
                          type(function), "and", type(iterable))
            return nil

    def MAPREGEX(self, lhs, regex, string):
        """Map function over regex matches in string."""
        # The arguments are reversible to enable things like sMR:xf
        if isinstance(regex, Block):
            lhs, regex = regex, lhs
        elif isinstance(string, Block):
            lhs, string = string, lhs
        if isinstance(regex, Scalar) and isinstance(string, Pattern):
            regex, string = string, regex
        elif isinstance(regex, Scalar):
            regex = self.REGEX(regex)
        if (isinstance(lhs, Block)
                and isinstance(regex, Pattern)
                and isinstance(string, Scalar)):
            matches = regex.asRegex().finditer(str(string))
            result = List()
            for matchObj in matches:
                groups = self.assignRegexVars(matchObj)
                result.append(self.functionCall(lhs, groups))
            return result
        else:
            self.err.warn("Unimplemented argtypes for MAPREGEX:",
                          type(lhs), type(regex), "and", type(string))
            return nil

    def MAPSUM(self, function, iterable):
        """Same as MAP, but sum the result afterwards.

        a MS b == $+(a M b)
        """
        result = Scalar("0")
        plus = ops.opsByArity[2]["+"]
        for item in self.MAP(function, iterable):
            result = self.evaluate([plus, result, item])
        return result

    def MAPUNPACK(self, function, iterable):
        """Map function over iterable, each item being a list of arguments.

        Equivalent to Python's itertools.starmap().
        """
        if isinstance(iterable, Block) and isinstance(function, PipIterable):
            # The arguments are reversible to enable things like lMU:f
            function, iterable = iterable, function
        if isinstance(iterable, PipIterable) and isinstance(function, Block):
            result = List()
            for item in iterable:
                if isinstance(item, PipIterable):
                    if item.isFinite():
                        arglist = list(item)
                    else:
                        # The item is an infinite Range
                        self.err.warn("Cannot unpack infinite Range in "
                                      "MAPUNPACK")
                        arglist = None
                else:
                    self.err.warn(f"Cannot unpack {type(item)} value in "
                                  "MAPUNPACK")
                    arglist = None
                if arglist is not None:
                    result.append(self.functionCall(function, arglist))
                else:
                    result.append(nil)
            return result
        else:
            self.err.warn("Unimplemented argtypes for MAPUNPACK:",
                          type(function), "and", type(iterable))
            return nil

    def MAPZIP(self, lhs, iterable1, iterable2):
        """Map function over the items of two iterables in parallel."""
        if isinstance(iterable1, Block) and isinstance(lhs, PipIterable):
            # The arguments are reversible to enable things like lMZ:fm
            lhs, iterable1 = iterable1, lhs
        if (isinstance(iterable1, PipIterable)
                and isinstance(iterable2, PipIterable)
                and isinstance(lhs, Block)):
            return List(self.functionCall(lhs, [item1, item2])
                        for item1, item2 in zip(iterable1, iterable2))
        else:
            self.err.warn("Unimplemented argtypes for MAPZIP:",
                          type(lhs), type(iterable1), "and", type(iterable2))
            return nil

    def MAX(self, iterable):
        """Return numeric maximum of iterable."""
        if isinstance(iterable, PipIterable):
            if iterable.isEmpty():
                self.err.warn("Cannot take MAX of an empty sequence")
                return nil
            elif not iterable.isFinite():
                self.err.warn("Cannot take MAX of an infinite Range")
                return nil
            else:
                try:
                    return max(iterable, key=lambda x:x.toNumber())
                except AttributeError:
                    self.err.warn("Argument to MAX contains non-numeric "
                                  f"value: {iterable!r}")
                    return nil
                except TypeError:
                    self.err.warn("Argument to MAX contains unorderable "
                                  f"types: {iterable!r}")
                    return nil
        else:
            self.err.warn("Unimplemented argtype for MAX:", type(iterable))
            return nil

    def MAXKEYED(self, keyFunction, iterable):
        """Return maximum by value of key function applied to each item.

        The key function is expected to return a number. It can also
        work if it returns a List, as long as it always returns a
        List with the same structure. Finite Ranges also work.
        """
        if not isinstance(keyFunction, Block) and isinstance(iterable, Block):
            keyFunction, iterable = iterable, keyFunction
        if (isinstance(keyFunction, Block)
                and isinstance(iterable, PipIterable)):
            def pyKey(enumitem):
                index, item = enumitem
                keyValue = self.functionCall(keyFunction,
                                             [item, Scalar(index)])
                if isinstance(keyValue, (Pattern, Block, Nil)):
                    self.err.warn("Replacing key value of type "
                                  f"{type(keyValue)} with 0 in MAXKEYED")
                    return 0
                elif (isinstance(keyValue, Range)
                      and keyValue.getUpper() is None):
                    self.err.warn("Replacing infinite Range key value "
                                  "with [] in MAXKEYED")
                    return []
                else:
                    # Convert Scalars to numbers, (nested) Lists to
                    # (nested) lists of numbers, Ranges to lists of numbers
                    return keyValue.toNumber()
            if iterable.isFinite():
                try:
                    index, item = max(enumerate(iterable), key=pyKey)
                    return item
                except TypeError:
                    self.err.warn("MAXKEYED cannot compare numbers to lists")
                    return nil
            else:
                self.err.warn("Cannot take MAXKEYED of an infinite Range")
                return nil
        else:
            self.err.warn("Unimplemented argtypes for MAXKEYED:",
                          type(keyFunction), "and", type(iterable))
            return nil

    def MIN(self, iterable):
        """Return numeric minimum of iterable."""
        if isinstance(iterable, PipIterable):
            if iterable.isEmpty():
                self.err.warn("Cannot take MIN of an empty sequence")
                return nil
            elif not iterable.isFinite():
                # Assuming that Ranges are ordered ascending, the minimum
                # value of an infinite Range is the first value
                return iterable[0]
            else:
                try:
                    return min(iterable, key=lambda x:x.toNumber())
                except AttributeError:
                    self.err.warn("Argument to MIN contains non-numeric "
                                  f"value: {iterable!r}")
                    return nil
                except TypeError:
                    self.err.warn("Argument to MIN contains unorderable "
                                  f"types: {iterable!r}")
                    return nil
        else:
            self.err.warn("Unimplemented argtype for MIN:", type(iterable))
            return nil

    def MINKEYED(self, keyFunction, iterable):
        """Return minimum by value of key function applied to each item.

        The key function is expected to return a number. It can also
        work if it returns a List, as long as it always returns a
        List with the same structure. Finite Ranges also work.
        """
        if not isinstance(keyFunction, Block) and isinstance(iterable, Block):
            keyFunction, iterable = iterable, keyFunction
        if (isinstance(keyFunction, Block)
                and isinstance(iterable, PipIterable)):
            def pyKey(enumitem):
                index, item = enumitem
                keyValue = self.functionCall(keyFunction,
                                             [item, Scalar(index)])
                if isinstance(keyValue, (Pattern, Block, Nil)):
                    self.err.warn("Replacing key value of type "
                                  f"{type(keyValue)} with 0 in MINKEYED")
                    return 0
                elif (isinstance(keyValue, Range)
                      and keyValue.getUpper() is None):
                    self.err.warn("Replacing infinite Range key value "
                                  "with [] in MINKEYED")
                    return []
                else:
                    # Convert Scalars to numbers, (nested) Lists to
                    # (nested) lists of numbers, Ranges to lists of numbers
                    return keyValue.toNumber()
            if iterable.isFinite():
                try:
                    index, item = min(enumerate(iterable), key=pyKey)
                    return item
                except TypeError:
                    self.err.warn("MINKEYED cannot compare numbers to lists")
                    return nil
            else:
                self.err.warn("Cannot take MINKEYED of an infinite Range")
                return nil
        else:
            self.err.warn("Unimplemented argtypes for MINKEYED:",
                          type(keyFunction), "and", type(iterable))
            return nil
    
    def MOD(self, lhs, rhs=None):
        if rhs is None:
            # Unary version takes its argument mod 2
            rhs = SCALAR_TWO
        if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
            try:
                result = lhs.toNumber() % rhs.toNumber()
                return Scalar(result)
            except ZeroDivisionError:
                self.err.warn("Modulo by zero")
                return nil
        else:
            self.err.warn("Unimplemented argtypes for MOD:",
                          type(lhs), "and", type(rhs))
            return nil

    def MUL(self, lhs, rhs):
        if isinstance(lhs, Scalar) and isinstance(rhs, Pattern):
            lhs, rhs = rhs, lhs
        if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
            result = lhs.toNumber() * rhs.toNumber()
            return Scalar(result)
        elif isinstance(lhs, Pattern) and isinstance(rhs, Scalar):
            # * with a Pattern and a Scalar returns a new Pattern that matches
            # the original regex repeated rhs times
            return lhs.wrap().repeat(int(rhs))
        else:
            self.err.warn("Unimplemented argtypes for MUL:",
                          type(lhs), "and", type(rhs))
            return nil

    def NATURALLOG(self, number):
        """Take the natural logarithm of number."""
        if isinstance(number, Scalar):
            if number.toNumber() > 0:
                result = math.log(number.toNumber())
                return Scalar(result)
            else:
                self.err.warn("Can't take logarithm of nonpositive number",
                              number)
                return nil
        else:
            self.err.warn("Unimplemented argtype for NATURALLOG:",
                          type(number))
            return nil

    def NEG(self, rhs):
        # TODO: Range with negative step value
        if isinstance(rhs, Scalar):
            result = -rhs.toNumber()
            return Scalar(result)
        elif isinstance(rhs, Pattern):
            # - operator on a Pattern makes it case-insensitive
            return Pattern(rhs, re.IGNORECASE)
        else:
            self.err.warn("Unimplemented argtype for NEG:", type(rhs))
            return nil

    def NOT(self, rhs):
        result = not rhs
        return Scalar(result)

    def NOTIN(self, needle, haystack):
        if isinstance(needle, (List, Range)) and isinstance(haystack, Scalar):
            return List(self.NOTIN(item, haystack) for item in needle)
        elif isinstance(needle, Pattern) and isinstance(haystack, Scalar):
            matchExists = needle.asRegex().search(str(haystack))
            return Scalar(not matchExists)
        else:
            return Scalar(needle not in haystack)

    def NUMCMP(self, lhs, rhs):
        # Equivalent to Python2's cmp() function: return -1 if lhs < rhs,
        # 0 if equal, 1 if lhs > rhs
        # Here we can just piggyback off the Pip numeric comparison operators
        if self.NUMGREATER(lhs, rhs):
            return Scalar("1")
        elif self.NUMLESS(lhs, rhs):
            return Scalar("-1")
        else:
            return Scalar("0")

    def NUMEQUAL(self, lhs, rhs):
        if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
            result = lhs.toNumber() == rhs.toNumber()
        elif isinstance(lhs, Range) and isinstance(rhs, Range):
            result = ((lhs.getLower() or 0) == (rhs.getLower() or 0)
                      and lhs.getUpper() == rhs.getUpper())
        elif (isinstance(lhs, (List, Range))
              and isinstance(rhs, (List, Range))):
            if lhs.isFinite() and rhs.isFinite():
                result = (len(lhs) == len(rhs)
                          and all(self.NUMEQUAL(i, j)
                                  for i, j in zip(lhs, rhs)))
            else:
                # We're comparing an infinite Range to a (finite) List
                result = False
        else:
            # Any other types are equal if they are identical
            result = lhs == rhs
        return Scalar(result)

    def NUMGREATER(self, lhs, rhs):
        if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
            result = lhs.toNumber() > rhs.toNumber()
        elif isinstance(lhs, Range) and isinstance(rhs, Range):
            leftLower = lhs.getLower() or 0
            rightLower = rhs.getLower() or 0
            leftUpper = lhs.getUpper()
            rightUpper = rhs.getUpper()
            if leftLower > rightLower:
                result = True
            elif leftLower < rightLower:
                result = False
            elif leftUpper == rightUpper:
                result = False
            elif not lhs.isFinite():
                # lhs is an infinite Range, thus bigger
                result = True
            elif not rhs.isFinite():
                # rhs is an infinite Range, thus bigger
                result = False
            else:
                result = leftUpper > rightUpper
        elif (isinstance(lhs, (List, Range))
              and isinstance(rhs, (List, Range))):
            result = None
            # TODO: some other way of clipping to the shorter length
            # that doesn't give the Iterating over an infinite Range
            # warning?
            for i, j in zip(lhs, rhs):
                if self.NUMGREATER(i, j):
                    result = True
                    break
                elif self.NUMGREATER(j, i):
                    result = False
                    break
            if result is None:
                # The two lists were equal as far as they went... but are they
                # the same length?
                if lhs.isFinite() and rhs.isFinite():
                    result = len(lhs) > len(rhs)
                elif not lhs.isFinite():
                    # lhs is an infinite Range, thus bigger
                    result = True
                elif not rhs.isFinite():
                    # rhs is an infinite Range, thus bigger
                    result = False
        else:
            result = False
        return Scalar(result)

    def NUMGREATEREQ(self, lhs, rhs):
        if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
            result = lhs.toNumber() >= rhs.toNumber()
        elif isinstance(lhs, Range) and isinstance(rhs, Range):
            leftLower = lhs.getLower() or 0
            rightLower = rhs.getLower() or 0
            leftUpper = lhs.getUpper()
            rightUpper = rhs.getUpper()
            if leftLower > rightLower:
                result = True
            elif leftLower < rightLower:
                result = False
            elif leftUpper == rightUpper:
                result = True
            elif not lhs.isFinite():
                # lhs is an infinite Range, thus bigger
                result = True
            elif not rhs.isFinite():
                # rhs is an infinite Range, thus bigger
                result = False
            else:
                result = leftUpper > rightUpper
        elif (isinstance(lhs, (List, Range))
              and isinstance(rhs, (List, Range))):
            result = None
            # TODO: some other way of clipping to the shorter length
            # that doesn't give the Iterating over an infinite Range
            # warning?
            for i, j in zip(lhs, rhs):
                if self.NUMGREATER(i, j):
                    result = True
                    break
                elif self.NUMGREATER(j, i):
                    result = False
                    break
            if result is None:
                # The two lists were equal as far as they went... but are they
                # the same length?
                if lhs.isFinite() and rhs.isFinite():
                    result = len(lhs) >= len(rhs)
                elif not lhs.isFinite():
                    # lhs is an infinite Range, thus bigger
                    result = True
                elif not rhs.isFinite():
                    # rhs is an infinite Range, thus bigger
                    result = False
        else:
            # For non-comparable types, the only way they can be >=
            # is if they are identical and thus equal
            result = lhs == rhs
        return Scalar(result)

    def NUMLESS(self, lhs, rhs):
        if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
            result = lhs.toNumber() < rhs.toNumber()
        elif isinstance(lhs, Range) and isinstance(rhs, Range):
            leftLower = lhs.getLower() or 0
            rightLower = rhs.getLower() or 0
            leftUpper = lhs.getUpper()
            rightUpper = rhs.getUpper()
            if leftLower < rightLower:
                result = True
            elif leftLower > rightLower:
                result = False
            elif leftUpper == rightUpper:
                result = False
            elif not lhs.isFinite():
                # lhs is an infinite Range, thus bigger
                result = False
            elif not rhs.isFinite():
                # rhs is an infinite Range, thus bigger
                result = True
            else:
                result = leftUpper < rightUpper
        elif (isinstance(lhs, (List, Range))
              and isinstance(rhs, (List, Range))):
            result = None
            # TODO: some other way of clipping to the shorter length
            # that doesn't give the Iterating over an infinite Range
            # warning?
            for i, j in zip(lhs, rhs):
                if self.NUMLESS(i, j):
                    result = True
                    break
                elif self.NUMLESS(j, i):
                    result = False
                    break
            if result is None:
                # The two lists were equal as far as they went... but are they
                # the same length?
                if lhs.isFinite() and rhs.isFinite():
                    result = len(lhs) < len(rhs)
                elif not lhs.isFinite():
                    # lhs is an infinite Range, thus bigger
                    result = False
                elif not rhs.isFinite():
                    # rhs is an infinite Range, thus bigger
                    result = True
        else:
            result = False
        return Scalar(result)

    def NUMLESSEQ(self, lhs, rhs):
        if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
            result = lhs.toNumber() <= rhs.toNumber()
        elif isinstance(lhs, Range) and isinstance(rhs, Range):
            leftLower = lhs.getLower() or 0
            rightLower = rhs.getLower() or 0
            leftUpper = lhs.getUpper()
            rightUpper = rhs.getUpper()
            if leftLower < rightLower:
                result = True
            elif leftLower > rightLower:
                result = False
            elif leftUpper == rightUpper:
                result = True
            elif not lhs.isFinite():
                # lhs is an infinite Range, thus bigger
                result = False
            elif not rhs.isFinite():
                # rhs is an infinite Range, thus bigger
                result = True
            else:
                result = leftUpper < rightUpper
        elif (isinstance(lhs, (List, Range))
              and isinstance(rhs, (List, Range))):
            result = None
            # TODO: some other way of clipping to the shorter length
            # that doesn't give the Iterating over an infinite Range
            # warning?
            for i, j in zip(lhs, rhs):
                if self.NUMLESS(i, j):
                    result = True
                    break
                elif self.NUMLESS(j, i):
                    result = False
                    break
            if result is None:
                # The two lists were equal as far as they went... but are they
                # the same length?
                if lhs.isFinite() and rhs.isFinite():
                    result = len(lhs) <= len(rhs)
                elif not lhs.isFinite():
                    # lhs is an infinite Range, thus bigger
                    result = False
                elif not rhs.isFinite():
                    # rhs is an infinite Range, thus bigger
                    result = True
        else:
            # For non-comparable types, the only way they can be <=
            # is if they are identical and thus equal
            result = lhs == rhs
        return Scalar(result)

    def NUMNOTEQUAL(self, lhs, rhs):
        if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
            result = lhs.toNumber() != rhs.toNumber()
        elif isinstance(lhs, Range) and isinstance(rhs, Range):
            result = ((lhs.getLower() or 0) != (rhs.getLower() or 0)
                      or lhs.getUpper() != rhs.getUpper())
        elif (isinstance(lhs, (List, Range))
              and isinstance(rhs, (List, Range))):
            if lhs.isFinite() and not rhs.isFinite():
                result = True
            elif not lhs.isFinite() and rhs.isFinite():
                result = True
            else:
                result = (len(lhs) != len(rhs)
                          or any(self.NUMNOTEQUAL(i, j)
                                  for i, j in zip(lhs, rhs)))
        else:
            result = not (lhs == rhs)
        return Scalar(result)

    def OBJEQUAL(self, lhs, rhs):
        return Scalar(lhs == rhs)

    def ONEGRID(self, rows, cols=None):
        if cols is None:
            cols = rows
        if isinstance(rows, Scalar) and isinstance(cols, Scalar):
            rows = range(int(rows))
            cols = range(int(cols))
            return List(List(Scalar("1") for col in cols)
                        for row in rows)
        else:
            self.err.warn("Unimplemented argtypes for ONEGRID:",
                          type(rows), "and", type(cols))
            return nil

    def OR(self, lhs, rhs):
        # Short-circuiting OR operator
        result = self.getRval(lhs)
        if not result:
            # The lhs was false, so we need to check the rhs
            result = self.getRval(rhs)
        return result
    
    def OUTPUT(self, expression):
        """Output an expression, NO trailing newline, and pass it through."""
        expression = self.getRval(expression)
        # Printing nil has no effect, including on whitespace
        if expression is not nil:
            print(self.STR(expression), end="")
        return expression

    def PALINDROMIZE(self, iterable):
        """Concatenate iterable with all but first element of its reverse.

        Empty List/Range or Scalar results in empty List or Scalar.
        """
        if isinstance(iterable, (Range, List)):
            if iterable.isEmpty():
                return List()
            elif iterable.isFinite():
                iterable = list(iterable)
                return List(iterable + iterable[-2::-1])
            else:
                self.err.warn("Cannot PALINDROMIZE an infinite Range")
                return nil
        elif isinstance(iterable, Scalar):
            if iterable.isEmpty():
                return Scalar()
            else:
                iterable = str(iterable)
                return Scalar(iterable + iterable[-2::-1])
        else:
            self.err.warn("Unimplemented argtype for PALINDROMIZE:",
                          type(iterable))
            return nil
    
    def PARENTHESIZE(self, expr):
        # Result of wrapping a single expression in parentheses
        return expr

    def PERMUTATIONS(self, iterable):
        """Return List of all permutations of iterable."""
        if isinstance(iterable, PipIterable):
            if iterable.isFinite():
                result = itertools.permutations(iterable)
                if isinstance(iterable, Scalar):
                    return List(self.JOIN(perm) for perm in result)
                else:
                    return List(List(perm) for perm in result)
            else:
                self.err.warn("Cannot get PERMUTATIONS of an infinite "
                              "Range")
                return nil
        else:
            self.err.warn("Unimplemented argtype for PERMUTATIONS:",
                          type(iterable))
            return nil

    def PICK(self, iterable, index):
        index = self.getRval(index)
        if isinstance(index, Scalar):
            index = int(index)
        else:
            # TODO: Allow List and Range indices
            self.err.warn("Unimplemented right argument type for PICK:",
                          type(index))
            return nil
        iterVal = self.getRval(iterable)
        if isinstance(iterVal, List):
            if iterVal.isEmpty():
                self.err.warn("Cannot PICK from empty List")
                return nil
            else:
                iterVal = list(iterVal)
                index %= len(iterVal)
                item = iterVal[index]
                iterVal = List(iterVal[:index] + iterVal[index+1:])
        elif isinstance(iterVal, Range):
            lower = iterVal.getLower() or 0
            if iterVal.isEmpty():
                self.err.warn("Cannot PICK from empty Range")
                return nil
            elif iterVal.isFinite():
                index %= len(iterVal)
                if index == 0:
                    item = iterVal[0]
                    iterVal = Range(lower + 1, iterVal.getUpper())
                else:
                    iterVal = list(iterVal)
                    item = iterVal[index]
                    iterVal = List(iterVal[:index] + iterVal[index+1:])
            else:
                # We can PICK from an infinite Range, but only if it's
                # the first value so the result is another Range
                if index == 0:
                    item = iterVal[0]
                    iterVal = Range(lower + 1, nil)
                else:
                    self.err.warn("Cannot PICK from middle of infinite Range")
                    return nil
        elif isinstance(iterVal, Scalar):
            if iterVal.isEmpty():
                self.err.warn("Cannot PICK from empty Scalar")
                return nil
            else:
                item = iterVal[index]
                iterVal = str(iterVal)
                index %= len(iterVal)
                iterVal = Scalar(iterVal[:index] + iterVal[index+1:])
        else:
            self.err.warn("Unimplemented left argument type for PICK:",
                          type(iterVal))
            return nil
        if isinstance(iterable, Lval):
            if not isinstance(iterable.base, List):
                self.assign(iterable, iterVal)
        else:
            self.err.warn("Picking from non-lvalue", iterable)
        return item

    def POP(self, iterable):
        iterVal = self.getRval(iterable)
        if isinstance(iterVal, List):
            if iterVal.isEmpty():
                self.err.warn("Cannot POP from empty List")
                return nil
            else:
                item = iterVal[0]
                iterVal = iterVal[1:]
        elif isinstance(iterVal, Range):
            lower = iterVal.getLower() or 0
            if iterVal.isEmpty():
                self.err.warn("Cannot POP from empty Range")
                return nil
            elif iterVal.isFinite():
                iterVal = Range(lower + 1, iterVal.getUpper())
            else:
                # Infinite Range
                iterVal = Range(lower + 1, nil)
            item = Scalar(lower)
        elif isinstance(iterVal, Scalar):
            if iterVal.isEmpty():
                self.err.warn("Cannot POP from empty Scalar")
                return nil
            else:
                item = iterVal[0]
                iterVal = iterVal[1:]
        else:
            self.err.warn("Unimplemented argtype for POP:", type(iterVal))
            return nil
        if isinstance(iterable, Lval):
            if not isinstance(iterable.base, List):
                self.assign(iterable, iterVal)
        else:
            self.err.warn("Popping from non-lvalue", iterable)
        return item

    def POS(self, rhs):
        if isinstance(rhs, Scalar):
            result = rhs.toNumber()
            return Scalar(result)
        elif isinstance(rhs, Range):
            return rhs
        elif isinstance(rhs, Pattern):
            # Given a Pattern, + applies regex + to the whole thing
            return rhs.wrap().plus()
        else:
            self.err.warn("Unimplemented argtype for POS:", type(rhs))
            return nil

    def POW(self, lhs, rhs=None):
        if rhs is None:
            # Unary **a is short for 2**a
            rhs = lhs
            lhs = SCALAR_TWO
        if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
            lhs = lhs.toNumber()
            rhs = rhs.toNumber()
            try:
                result = lhs ** rhs
            except ZeroDivisionError:
                self.err.warn("Can't raise zero to negative power")
                return nil
            if lhs < 0 and int(rhs) != rhs:
                # Negative number to fractional power would be a complex
                # number; for now, return nil
                self.err.warn("Can't raise negative number to "
                              "fractional power")
                return nil
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtypes for POW:",
                          type(lhs), "and", type(rhs))
            return nil

    def POWEROFTEN(self, lhs, rhs=None):
        """E-notation: aEEb == a*10**b."""
        if rhs is None:
            # Unary EEa is short for 1EEa
            rhs = lhs
            lhs = SCALAR_ONE
        if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
            lhs = lhs.toNumber()
            rhs = rhs.toNumber()
            result = lhs * 10 ** rhs
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtypes for POWEROFTEN:",
                          type(lhs), "and", type(rhs))
            return nil

    def PREFIX(self, lhs, rhs=None):
        if rhs is None:
            # The unary version gives all but the rightmost character
            rhs = Scalar(-1)
        elif isinstance(rhs, Lval):
            rhs = self.getRval(rhs)
        index = None
        if isinstance(rhs, (List, Range)):
            if isinstance(lhs, Lval):
                lhs = self.getRval(lhs)
            return List(self.PREFIX(lhs, length) for length in rhs)
        elif isinstance(rhs, Scalar):
            sliceLength = int(rhs)
            # Slice from the beginning to sliceLength
            index = slice(None, sliceLength)
        if isinstance(lhs, Lval) and index is not None:
            return Lval(lhs, index)
        elif isinstance(lhs, PipIterable) and index is not None:
            # Use the lhs's __getitem__ with a slice argument
            if not lhs.isFinite() and sliceLength < 0:
                self.err.warn("Cannot slice from end of infinite Range")
                return nil
            else:
                return lhs[index]
        else:
            self.err.warn("Unimplemented argtypes for PREFIX:",
                          type(lhs), "and", type(rhs))
            return nil

    def PREPENDELEM(self, lhs, rhs):
        # Note the order of operands: lhs is the list
        if isinstance(lhs, (Scalar, Pattern, Nil)):
            lhs = List([lhs])
        if isinstance(lhs, (List, Range)):
            if lhs.isFinite():
                return List([rhs] + list(lhs))
            else:
                self.err.warn("Cannot PREPENDELEM to infinite Range")
                return nil
        else:
            self.err.warn("Unimplemented argtypes for PREPENDELEM:",
                          type(lhs), "and", type(rhs))
            return nil
    
    def PRINT(self, expression):
        """Output an expression with trailing newline and pass it through.

        Printing nil has no effect, including on whitespace.
        """
        if expression is not nil:
            print(self.STR(expression))
        return expression

    def PUSH(self, iterable, item):
        """Push the rhs onto the front of lhs in place."""
        item = self.getRval(item)
        iterVal = self.getRval(iterable)
        if isinstance(iterVal, (List, Range)):
            if iterVal.isFinite():
                iterVal = self.PREPENDELEM(iterVal, item)
            else:
                self.err.warn("Cannot PUSH to infinite Range")
                return nil
        elif isinstance(iterVal, (Scalar, Pattern)):
            iterVal = self.CAT(Scalar(item), iterVal)
        elif isinstance(iterVal, Nil):
            iterVal = List([item])
        if isinstance(iterable, Lval):
            if not isinstance(iterable.base, List):
                self.assign(iterable, iterVal)
                return iterable
            else:
                return iterVal
        else:
            self.err.warn("Pushing to non-lvalue", iterable)
            return iterVal

    def PUSHBACK(self, iterable, item):
        """Push the rhs onto the back of lhs in place."""
        item = self.getRval(item)
        iterVal = self.getRval(iterable)
        if isinstance(iterVal, (List, Range)):
            if iterVal.isFinite():
                iterVal = self.APPENDELEM(iterVal, item)
            else:
                self.err.warn("Cannot PUSHBACK to infinite Range")
                return nil
        elif isinstance(iterVal, (Scalar, Pattern)):
            iterVal = self.CAT(iterVal, Scalar(item))
        elif isinstance(iterVal, Nil):
            iterVal = List([item])
        if isinstance(iterable, Lval):
            if not isinstance(iterable.base, List):
                self.assign(iterable, iterVal)
                return iterable
            else:
                return iterVal
        else:
            self.err.warn("Pushing to non-lvalue", iterable)
            return iterVal

    def QUADREFLECT(self, iterable):
        """REFLECT the iterable and also each of its elements."""
        result = self.REFLECT(iterable)
        if result is not nil:
            return List(self.REFLECT(row) for row in result)
        else:
            return nil

    def QUADPALINDROMIZE(self, iterable):
        """PALINDROMIZE the iterable and also each of its elements."""
        result = self.PALINDROMIZE(iterable)
        if result is not nil:
            return List(self.PALINDROMIZE(row) for row in result)
        else:
            return nil

    def RADIANS(self, rhs):
        """Convert from degrees to radians."""
        if isinstance(rhs, Scalar):
            result = rhs.toNumber() / 180 * math.pi
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtype for RADIANS:", type(rhs))
            return nil

    def RANDCHOICE(self, iterable):
        if isinstance(iterable, PipIterable):
            if iterable.isEmpty():
                self.err.warn("Cannot take RANDCHOICE from empty iterable")
                return nil
            elif iterable.isFinite():
                index = random.randrange(len(iterable))
                return iterable[index]
            else:
                self.err.warn("Cannot take RANDCHOICE from infinite Range")
        else:
            self.err.warn("Unimplemented argtype for RANDCHOICE:",
                          type(iterable))
            return nil

    def RANDRANGE(self, lhs, rhs):
        if isinstance(lhs, (Scalar, Nil)) and isinstance(rhs, Scalar):
            if lhs is nil:
                lhs = 0
            else:
                lhs = int(lhs)
            rhs = int(rhs)
            return Scalar(random.randrange(lhs, rhs))
        else:
            self.err.warn("Unimplemented argtypes for RANDRANGE:",
                          type(lhs), "and", type(rhs))
            return nil
        
    def RANDRANGETO(self, rhs):
        """Unary version of RANDRANGE."""
        if isinstance(rhs, Scalar):
            return Scalar(random.randrange(int(rhs)))
        else:
            self.err.warn("Unimplemented argtype for RANDRANGETO:",
                          type(rhs))
            return nil

    def RANGE(self, lhs, rhs):
        if (isinstance(lhs, (Scalar, Nil))
                and isinstance(rhs, (Scalar, Nil))):
            return Range(lhs, rhs)
        elif isinstance(lhs, Pattern) and isinstance(rhs, Pattern):
            # , with two Patterns returns a new Pattern that matches one OR
            # the other
            return lhs.wrap().alternate(rhs.wrap())
        else:
            self.err.warn("Unimplemented argtypes for RANGE:",
                          type(lhs), "and", type(rhs))
            return nil

    def RANGETO(self, rhs):
        """Unary version of RANGE."""
        if isinstance(rhs, (Scalar, Nil)):
            return Range(nil, rhs)
        elif isinstance(rhs, Pattern):
            # , operator on a Pattern makes ^ and $ match fronts & ends of
            # lines
            return Pattern(rhs, re.MULTILINE)
        else:
            self.err.warn("Unimplemented argtype for RANGETO:", type(rhs))
            return nil

    def RECURSE(self, rhs):
        "Call the current function recursively with rhs as its argument."
        return self.functionCall(self.localScope.function,
                                 [rhs])

    def REFLECT(self, iterable):
        """Concatenate iterable with its reverse."""
        if isinstance(iterable, (Range, List)):
            if iterable.isFinite():
                iterable = list(iterable)
                return List(iterable + iterable[::-1])
            else:
                self.err.warn("Cannot REFLECT an infinite Range")
                return nil
        elif isinstance(iterable, Scalar):
            iterable = str(iterable)
            return Scalar(iterable + iterable[::-1])
        else:
            self.err.warn("Unimplemented argtype for REFLECT:",
                          type(iterable))
            return nil

    def REGEX(self, rhs):
        """Convert Scalar, List, or Range to properly-escaped Pattern."""
        if isinstance(rhs, Pattern):
            return rhs
        elif isinstance(rhs, Scalar):
            return Pattern(re.escape(str(rhs))).wrap()
        elif isinstance(rhs, Range):
            if rhs.isEmpty():
                return Pattern(r"\b\B").wrap()
            elif rhs.isFinite():
                return Pattern("|".join(re.escape(str(item))
                                        for item in rhs)).wrap()
            else:
                self.err.warn("Cannot take REGEX of infinite Range")
                return nil
        elif isinstance(rhs, List):
            if rhs.isEmpty():
                return Pattern(r"\b\B").wrap()
            else:
                return Pattern("|".join(str(self.REGEX(item))
                                        for item in rhs)).wrap()
        else:
            self.err.warn("Unimplemented argtype for REGEX:", type(rhs))
            return nil

    def REPEATLIST(self, lhs, rhs):
        if isinstance(lhs, (Scalar, Pattern, Nil)):
            lhs = List([lhs])
        if isinstance(lhs, (List, Range)) and isinstance(rhs, Scalar):
            if lhs.isFinite():
                return List(list(lhs.copy()) * int(rhs))
            else:
                self.err.warn("Cannot REPEATLIST an infinite Range")
                return nil
        else:
            self.err.warn("Unimplemented argtypes for REPEATLIST:",
                          type(lhs), "and", type(rhs))
            return nil

    def REPLACE(self, lhs, old, new):
        lhs = List(lhs) if isinstance(lhs, Range) else lhs
        old = List(old) if isinstance(old, Range) else old
        new = List(new) if isinstance(new, Range) else new
        if isinstance(old, Scalar) and isinstance(new, (Pattern, Block)):
            old = self.REGEX(old)
        if (isinstance(lhs, PipIterable)
                and isinstance(old, (PipIterable, Pattern))
                and isinstance(new, (PipIterable, Pattern, Block, Nil))):
            if isinstance(lhs, List):
                # Return a List of results
                return List(self.REPLACE(eachLhs, old, new)
                            for eachLhs in lhs)
            elif isinstance(old, List) and isinstance(new, List):
                # Both are lists--zip and replace parallel items
                result = lhs
                for eachOld, eachNew in zip(old, new):
                    result = self.REPLACE(result, eachOld, eachNew)
                # Items in the old list that don't correspond to items
                # in the new list should just be deleted
                for eachOld in old[len(new):]:
                    result = self.REPLACE(result, eachOld, nil)
                return result
            elif isinstance(old, List):
                # Replace each element of old with new
                result = lhs
                for eachOld in old:
                    result = self.REPLACE(result, eachOld, new)
                return result
            elif isinstance(old, Pattern):
                if isinstance(new, Pattern):
                    replacement = new.asReplacement()
                elif isinstance(new, PipIterable):
                    # If replacing with a literal string, escape all
                    # backslashes first
                    replacement = str(new).replace("\\", "\\\\")
                elif isinstance(new, Block):
                    # For each match of the pattern, call the function with
                    # the match and groups as arguments and substitute the
                    # result
                    # First we need to define a Python function that we can
                    # pass to re.sub()
                    def replacement(matchObj):
                        groups = self.assignRegexVars(matchObj)
                        retVal = self.functionCall(new, groups)
                        return str(retVal)
                elif new is nil:
                    replacement = ""
                result = old.asRegex().sub(replacement, str(lhs))
                return Scalar(result)
            elif isinstance(old, Scalar):
                if new is nil:
                    replacement = ""
                else:
                    # NB: this branch also covers the case where new is a List
                    # TBD: does that approach make the most sense?
                    replacement = str(new)
                result = str(lhs).replace(str(old), replacement)
                return Scalar(result)
        else:
            self.err.warn("Unimplemented argtypes for REPLACE:",
                          type(lhs), type(old), "and", type(new))
            return nil

    def REPLACEAT(self, lhs, index, new):
        if (isinstance(lhs, PipIterable)
                and isinstance(index, PipIterable)):
            result = lhs.copy()
            if isinstance(index, List):
                if isinstance(new, List):
                    # Both are lists--replace at corresponding indices
                    # with corresponding values
                    # If the new list is shorter than the index list, cycle
                    # the replacement values as necessary
                    for i, eachIndex in enumerate(index):
                        result = self.REPLACEAT(result, eachIndex, new[i])
                    return result
                else:
                    # Replace at each index with the same new
                    for eachIndex in index:
                        result = self.REPLACEAT(result, eachIndex, new)
                    return result
            else:
                if isinstance(index, Scalar):
                    index = int(index)
                elif isinstance(index, Range):
                    index = index.toSlice()
                result[index] = new
                return result
        elif index is nil:
            # Replacing at nil index leaves lhs unchanged
            return lhs
        else:
            self.err.warn("Unimplemented argtypes for REPLACEAT:",
                          type(lhs), type(index), "and", type(new))
            return nil

    def REMOVE(self, lhs, rhs):
        if isinstance(lhs, Scalar):
            if isinstance(rhs, Scalar):
                result = str(lhs).replace(str(rhs), "")
            elif isinstance(rhs, Pattern):
                result = rhs.asRegex().sub("", str(lhs))
            elif isinstance(rhs, PipIterable):
                result = lhs
                for item in rhs:
                    result = self.REMOVE(result, item)
            elif rhs is nil:
                # Nothing to remove, so return the original value
                result = lhs
            else:
                self.err.warn("Unimplemented argtypes for REMOVE:",
                              type(lhs), "and", type(rhs))
                return nil
            return Scalar(result)
        elif isinstance(lhs, PipIterable):
            if lhs.isFinite():
                result = list(lhs)
                while rhs in result:
                    # Loop is necessary because list.remove only removes the
                    # first instance
                    result.remove(rhs)
                return List(result)
            else:
                self.err.warn("Cannot REMOVE item(s) from infinite Range")
                return nil
        else:
            self.err.warn("Unimplemented argtypes for REMOVE:",
                          type(lhs), "and", type(rhs))
            return nil

    def REPR(self, rhs):
        return Scalar(repr(rhs))

    def REVERSE(self, rhs):
        if isinstance(rhs, Scalar):
            return Scalar(str(rhs)[::-1])
        elif isinstance(rhs, (Range, List)):
            if rhs.isFinite():
                rhs = list(rhs)
                return List(rhs[::-1])
            else:
                self.err.warn("Cannot REVERSE an infinite Range")
                return nil
        else:
            self.err.warn("Unimplemented argtype for REVERSE:", type(rhs))
            return nil

    def RIGHTOF(self, lhs, rhs=None):
        if rhs is None:
            # The unary version gives all but the leftmost character
            rhs = SCALAR_ONE
        if isinstance(rhs, Lval):
            rhs = self.getRval(rhs)
        if isinstance(lhs, Lval) and isinstance(rhs, Scalar):
            index = slice(int(rhs), None)
            return Lval(lhs, index)
        elif isinstance(rhs, (List, Range)):
            if isinstance(lhs, Lval):
                lhs = self.getRval(lhs)
            return List(self.RIGHTOF(lhs, index) for index in rhs)
        elif isinstance(lhs, PipIterable) and isinstance(rhs, Scalar):
            # Use the lhs's __getitem__ with a slice argument
            if not lhs.isFinite() and int(rhs) < 0:
                self.err.warn("Cannot slice from end of infinite Range")
                return nil
            else:
                return lhs[int(rhs):]
        else:
            self.err.warn("Unimplemented argtypes for RIGHTOF:",
                          type(lhs), "and", type(rhs))
            return nil

    def ROOT(self, lhs, rhs):
        if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
            lhs = lhs.toNumber()
            rhs = rhs.toNumber()
            if lhs == 0:
                self.err.warn("Zeroth root is not defined")
                return nil
            elif rhs < 0 and int(1/lhs) != 1/lhs:
                # Root of negative number would be a complex number; for now,
                # return nil
                self.err.warn("Cannot take root of negative number", rhs)
                return nil
            try:
                result = rhs ** (1 / lhs)
            except ZeroDivisionError:
                self.err.warn("Cannot take negative root of zero", lhs)
                return nil
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtypes for ROOT:",
                          type(lhs), "and", type(rhs))
            return nil

    def ROUNDNEAREST(self, lhs, rhs=None):
        """Round lhs to the nearest multiple of rhs."""
        if rhs is None:
            precision = 1
        elif isinstance(rhs, Scalar):
            precision = abs(rhs.toNumber())
        else:
            precision = None
        if isinstance(lhs, Scalar) and precision is not None:
            if precision == 0:
                return lhs
            elif precision == 1:
                lhs = lhs.toNumber()
                if math.remainder(lhs, 1) > 0:
                    return Scalar(math.floor(lhs))
                else:
                    return Scalar(math.ceil(lhs))
            else:
                lhs = lhs.toNumber()
                return Scalar(lhs - math.remainder(lhs, precision))
        else:
            if rhs is None:
                self.err.warn("Unimplemented argtype for ROUNDNEAREST:",
                              type(lhs))
            else:
                self.err.warn("Unimplemented argtypes for ROUNDNEAREST:",
                              type(lhs), "and", type(rhs))
            return nil

    def ROUNDZERO(self, lhs, rhs=None):
        """Round lhs to the nearest multiple of rhs closer to zero."""
        if rhs is None:
            precision = 1
        elif isinstance(rhs, Scalar):
            precision = abs(rhs.toNumber())
        else:
            precision = None
        if isinstance(lhs, Scalar) and precision is not None:
            if precision == 0:
                return lhs
            elif precision == 1:
                lhs = lhs.toNumber()
                return Scalar(math.trunc(lhs))
            else:
                lhs = lhs.toNumber()
                return Scalar(math.trunc(lhs / precision) * precision)
        else:
            if rhs is None:
                self.err.warn("Unimplemented argtype for ROUNDZERO:",
                              type(lhs))
            else:
                self.err.warn("Unimplemented argtypes for ROUNDZERO:",
                              type(lhs), "and", type(rhs))
            return nil

    def RSTRIP(self, string, extra=None):
        if extra is nil:
            return string
        elif (not isinstance(extra, (PipIterable, Pattern))
              and extra is not None):
            self.err.warn("Unimplemented argtype for rhs of RSTRIP:",
                          type(extra))
            return nil
            
        if isinstance(string, (List, Range)):
            return List(self.RSTRIP(item, extra) for item in string)
        elif isinstance(string, Scalar):
            if isinstance(extra, (List, Range)):
                for item in extra:
                    string = self.RSTRIP(string, item)
                return string
            elif isinstance(extra, Scalar):
                return Scalar(str(string).rstrip(str(extra)))
            elif isinstance(extra, Pattern):
                # Python doesn't have a regex strip operation--we have to
                # roll our own
                patternStr = str(extra)
                if patternStr == "":
                    return string
                oldString = str(string)
                end = re.compile(patternStr + "$")
                newString = end.sub("", oldString)
                while oldString != newString:
                    oldString = newString
                    newString = end.sub("", oldString)
                return Scalar(newString)
            elif extra is None:
                return Scalar(str(string).rstrip())
        else:
            self.err.warn("Unimplemented argtype for lhs of RSTRIP:",
                          type(string))
            return nil

    def RVALAND(self, lhs, rhs):
        return lhs and rhs

    def RVALIFTE(self, test, trueBranch, falseBranch):
        return trueBranch if test else falseBranch

    def RVALOR(self, lhs, rhs):
        return lhs or rhs

    def SECANT(self, rhs):
        if isinstance(rhs, Scalar):
            return Scalar(1 / math.cos(rhs.toNumber()))
        else:
            self.err.warn("Unimplemented argtype for SECANT:", type(rhs))
            return nil

    def SEND(self, head, *tail):
        # A send-expression's semantics depend on the type of the head:
        # - Block: function call
        # - List, Scalar, Range: subscript
        if isinstance(head, Lval):
            # Need to check whether it's actually a function
            headRval = self.getRval(head)
            if isinstance(headRval, Block):
                head = headRval
            # If not, leave it as an lval so the subscripted version can also
            # be an lval
        if isinstance(head, Block):
            return self.functionCall(head, tail)
        elif isinstance(head, (Lval, PipIterable)):
            value = head
            for index in tail:
                value = self.AT(value, index)
            return value
        else:
            self.err.warn("Unimplemented argtype for SEND:", type(head))
            return nil

    def SHUFFLE(self, iterable):
        """Return a random permutation of the iterable."""
        if isinstance(iterable, PipIterable):
            if iterable.isFinite():
                items = list(iterable)
                random.shuffle(items)
                if isinstance(iterable, Scalar):
                    return self.JOIN(items)
                else:
                    return List(items)
            else:
                self.err.warn("Cannot SHUFFLE infinite Range")
                return nil
        else:
            self.err.warn("Unimplemented argtype for SHUFFLE:",
                          type(iterable))
            return nil

    def SIGN(self, rhs):
        if isinstance(rhs, Scalar):
            rhs = rhs.toNumber()
            if rhs < 0:
                return Scalar("-1")
            elif rhs > 0:
                return Scalar("1")
            else:
                return Scalar("0")
        else:
            self.err.warn("Unimplemented argtype for SIGN:", type(rhs))
            return nil

    def SINE(self, rhs):
        if isinstance(rhs, Scalar):
            return Scalar(math.sin(rhs.toNumber()))
        else:
            self.err.warn("Unimplemented argtype for SINE:", type(rhs))
            return nil

    def SORTKEYED(self, keyFunction, iterable):
        """Sort by value of key function applied to each item.

        The key function is expected to return a number. It can also
        work if it returns a List, as long as it always returns a
        List with the same structure. Finite Ranges also work.
        """
        if not isinstance(keyFunction, Block) and isinstance(iterable, Block):
            keyFunction, iterable = iterable, keyFunction
        if (isinstance(keyFunction, Block)
                and isinstance(iterable, PipIterable)):
            def pyKey(enumitem):
                index, item = enumitem
                keyValue = self.functionCall(keyFunction,
                                             [item, Scalar(index)])
                if isinstance(keyValue, (Pattern, Block, Nil)):
                    self.err.warn("Replacing key value of type "
                                  f"{type(keyValue)} with 0 in SORTKEYED")
                    return 0
                elif (isinstance(keyValue, Range)
                      and keyValue.getUpper() is None):
                    self.err.warn("Replacing infinite Range key value "
                                  "with [] in SORTKEYED")
                    return []
                else:
                    # Convert Scalars to numbers, (nested) Lists to
                    # (nested) lists of numbers, Ranges to lists of numbers
                    return keyValue.toNumber()
            if iterable.isFinite():
                try:
                    return List(item
                                for index, item in sorted(enumerate(iterable),
                                                          key=pyKey))
                except TypeError:
                    self.err.warn("SORTKEYED cannot compare numbers to lists")
                    return nil
            else:
                self.err.warn("SORTKEYED cannot sort infinite Range")
                return nil
        else:
            self.err.warn("Unimplemented argtypes for SORTKEYED:",
                          type(keyFunction), "and", type(iterable))
            return nil

    def SORTNUM(self, iterable):
        if isinstance(iterable, PipIterable):
            def pyKey(item):
                if isinstance(item, (Pattern, Block, Nil)):
                    self.err.warn(f"Items of type {type(item)} "
                                  "are sorted as if they were 0 "
                                  "in SORTNUM")
                    return 0
                elif isinstance(item, Range) and item.getUpper() is None:
                    self.err.warn("Infinite Ranges are sorted as if "
                                  "they were [] in SORTNUM")
                    return []
                else:
                    # Treat Scalars as numbers, (nested) Lists as
                    # (nested) lists of numbers, Ranges as lists of numbers
                    return item.toNumber()
            if iterable.isFinite():
                try:
                    return List(sorted(iterable, key=pyKey))
                except TypeError:
                    self.err.warn("SORTNUM cannot compare numbers to lists")
                    return nil
            else:
                self.err.warn("Cannot sort infinite Range")
                return nil
        else:
            self.err.warn("Unimplemented argtype for SORTNUM:",
                          type(iterable))
            return nil

    def SORTSTRING(self, iterable):
        if isinstance(iterable, Scalar):
            return Scalar("".join(sorted(str(iterable))))
        elif isinstance(iterable, PipIterable):
            if iterable.isFinite():
                # This is going to get a bit wonky when sorting lists of
                # lists, but not sure it's worth the effort to fix
                return List(sorted(iterable, key=str))
            else:
                self.err.warn("Cannot sort infinite Range")
                return nil
        else:
            self.err.warn("Unimplemented argtype for SORTSTRING:",
                          type(iterable))
            return nil

    def SPLIT(self, string, sep=None):
        if isinstance(sep, Scalar):
            sep = str(sep)
        elif not isinstance(sep, Pattern) and sep is not None:
            # Some other type, not a valid separator
            self.err.warn("Unimplemented separator type for SPLIT:",
                          type(sep))
            return nil
        if isinstance(string, Scalar):
            if sep is None or sep == "":
                result = (Scalar(char) for char in str(string))
            elif isinstance(sep, Pattern):
                if str(sep) == "":
                    result = (Scalar(char) for char in str(string))
                else:
                    sep = sep.asSeparator()
                    result = (Scalar(substr)
                              for substr in sep.split(str(string)))
            else:
                result = (Scalar(substr) for substr in str(string).split(sep))
            return List(result)
        else:
            if sep is None:
                self.err.warn("Unimplemented argtype for SPLIT:",
                              type(string))
            else:
                self.err.warn("Unimplemented argtypes for SPLIT:",
                              type(string), "and", type(sep))
            return nil

    def SPLITAT(self, iterable, indices):
        """Split iterable at given indices."""
        if isinstance(indices, Scalar):
            indices = [int(indices)]
        elif isinstance(indices, Range):
            # TODO: Handle infinite Ranges better?
            if indices.isFinite:
                indices = [int(index) for index in indices]
            else:
                self.err.warn("Cannot SPLITAT infinite Range of indices")
                return nil
        elif isinstance(indices, List):
            try:
                indices = list(set(int(index) for index in indices))
            except TypeError:
                # The List contained items that couldn't be converted to int
                self.err.warn("List of indices for SPLITAT must contain "
                              "only Scalars")
                return nil

        if isinstance(iterable, PipIterable) and isinstance(indices, list):
            if iterable.isFinite():
                result = List()
                prevIndex = 0
                length = len(iterable)
                for i in range(length):
                    if i in indices or i - length in indices:
                        result.append(iterable[prevIndex:i])
                        prevIndex = i
                result.append(iterable[prevIndex:])
                return result
            else:
                self.err.warn("Cannot split infinite Range at indices")
                return nil
        else:
            self.err.warn("Unimplemented argtypes for SPLITAT:",
                          type(iterable), "and", type(indices))
            return nil

    def SQRT(self, rhs):
        if isinstance(rhs, Scalar):
            rhs = rhs.toNumber()
            if rhs < 0:
                # Square root of negative number would be a complex number;
                # for now, return nil
                self.err.warn("Can't take square root of negative number",
                              rhs)
                return nil
            result = rhs ** 0.5
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtype for SQRT:", type(rhs))
            return nil

    def SQUARE(self, rhs):
        if isinstance(rhs, Scalar):
            rhs = rhs.toNumber()
            result = rhs * rhs
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtype for SQUARE:", type(rhs))
            return nil

    def STR(self, rhs):
        return Scalar(str(rhs))

    def STREQUAL(self, lhs, rhs):
        if (isinstance(lhs, (Scalar, Pattern))
                and isinstance(rhs, (Scalar, Pattern))):
            result = str(lhs) == str(rhs)
        elif isinstance(lhs, List) and isinstance(rhs, List):
            result = (len(lhs) == len(rhs)
                      and all(self.STREQUAL(i, j)
                              for i, j in zip(lhs, rhs)))
        else:
            result = lhs == rhs
        return Scalar(result)

    def STRGREATER(self, lhs, rhs):
        if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
            result = str(lhs) > str(rhs)
        elif isinstance(lhs, List) and isinstance(rhs, List):
            # TODO: Handle Ranges like Lists?
            result = None
            for i, j in zip(lhs, rhs):
                if self.STRGREATER(i, j):
                    result = True
                    break
                elif self.STRGREATER(j, i):
                    result = False
                    break
            if result is None:
                # The two lists were equal as far as they went... but are they
                # the same length?
                result = len(lhs) > len(rhs)
        else:
            result = False
        return Scalar(result)

    def STRGREATEREQ(self, lhs, rhs):
        if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
            result = str(lhs) >= str(rhs)
        elif isinstance(lhs, List) and isinstance(rhs, List):
            result = None
            for i, j in zip(lhs, rhs):
                if self.STRGREATER(i, j):
                    result = True
                    break
                elif self.STRGREATER(j, i):
                    result = False
                    break
            if result is None:
                # The two lists were equal as far as they went... but are they
                # the same length?
                result = len(lhs) >= len(rhs)
        else:
            result = lhs == rhs
        return Scalar(result)

    def STRIP(self, string, extra=None):
        if extra is nil:
            return string
        elif (not isinstance(extra, (PipIterable, Pattern))
              and extra is not None):
            self.err.warn("Unimplemented argtype for rhs of STRIP:",
                          type(extra))
            return nil
            
        if isinstance(string, (List, Range)):
            return List(self.STRIP(item, extra) for item in string)
        elif isinstance(string, Scalar):
            if isinstance(extra, (List, Range)):
                for item in extra:
                    string = self.STRIP(string, item)
                return string
            elif isinstance(extra, Scalar):
                return Scalar(str(string).strip(str(extra)))
            elif isinstance(extra, Pattern):
                # Python doesn't have a regex strip operation--we have to
                # roll our own
                patternStr = str(extra)
                if patternStr == "":
                    return string
                oldString = str(string)
                beginning = re.compile("^" + patternStr)
                newString = beginning.sub("", oldString)
                while oldString != newString:
                    oldString = newString
                    newString = beginning.sub("", oldString)
                end = re.compile(patternStr + "$")
                newString = end.sub("", oldString)
                while oldString != newString:
                    oldString = newString
                    newString = end.sub("", oldString)
                return Scalar(newString)
            elif extra is None:
                return Scalar(str(string).strip())
        else:
            self.err.warn("Unimplemented argtype for lhs of STRIP:",
                          type(string))
            return nil

    def STRLESS(self, lhs, rhs):
        if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
            result = str(lhs) < str(rhs)
        elif isinstance(lhs, List) and isinstance(rhs, List):
            result = None
            for i, j in zip(lhs, rhs):
                if self.STRLESS(i, j):
                    result = True
                    break
                elif self.STRLESS(j, i):
                    result = False
                    break
            if result is None:
                # The two lists were equal as far as they went... but are they
                # the same length?
                result = len(lhs) < len(rhs)
        else:
            result = False
        return Scalar(result)

    def STRLESSEQ(self, lhs, rhs):
        if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
            result = str(lhs) <= str(rhs)
        elif isinstance(lhs, List) and isinstance(rhs, List):
            result = None
            for i, j in zip(lhs, rhs):
                if self.STRLESS(i, j):
                    result = True
                    break
                elif self.STRLESS(j, i):
                    result = False
                    break
            if result is None:
                # The two lists were equal as far as they went... but are they
                # the same length?
                result = len(lhs) <= len(rhs)
        else:
            result = lhs == rhs
        return Scalar(result)

    def STRMUL(self, lhs, rhs):
        if isinstance(lhs, (Scalar, Pattern)) and isinstance(rhs, Scalar):
            string = str(lhs)
            num = int(rhs)
            return type(lhs)(string*num)
        else:
            self.err.warn("Unimplemented argtypes for STRMUL:",
                          type(lhs), "and", type(rhs))
            return nil

    def STRNOTEQUAL(self, lhs, rhs):
        if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
            result = str(lhs) != str(rhs)
        elif isinstance(lhs, List) and isinstance(rhs, List):
            result = (len(lhs) != len(rhs)
                      or any(self.STRNOTEQUAL(i, j)
                              for i, j in zip(lhs, rhs)))
        else:
            result = not (lhs == rhs)
        return Scalar(result)

    def SUB(self, lhs, rhs):
        if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
            result = lhs.toNumber() - rhs.toNumber()
            return Scalar(result)
        elif isinstance(lhs, Range) and isinstance(rhs, Scalar):
            lower = lhs.getLower() or 0
            upper = lhs.getUpper()
            lower -= int(rhs)
            if upper is not None:
                upper -= int(rhs)
            else:
                upper = nil
            return Range(lower, upper)
        elif isinstance(lhs, Scalar) and isinstance(rhs, Range):
            if rhs.isFinite():
                return List(self.SUB(lhs, item) for item in rhs)
            else:
                self.err.warn("Cannot subtract infinite Range from Scalar")
                return nil
        elif isinstance(lhs, Range) and isinstance(rhs, Range):
            if lhs.isFinite() and rhs.isFinite():
                result = List()
                for a, b in itertools.zip_longest(lhs, rhs, fillvalue=None):
                    if a is None:
                        result.append(b)
                    elif b is None:
                        result.append(a)
                    else:
                        result.append(self.SUB(a, b))
                return result
            else:
                self.err.warn("Cannot subtract infinite Ranges")
                return nil
        else:
            self.err.warn("Unimplemented argtypes for SUB:",
                          type(lhs), "and", type(rhs))
            return nil

    def SUFFIX(self, lhs, rhs=None):
        if rhs is None:
            # The unary version gives all but the leftmost character
            rhs = Scalar(-1)
        elif isinstance(rhs, Lval):
            rhs = self.getRval(rhs)
        index = None
        if isinstance(rhs, (List, Range)):
            if isinstance(lhs, Lval):
                lhs = self.getRval(lhs)
            return List(self.SUFFIX(lhs, length) for length in rhs)
        elif isinstance(rhs, Scalar):
            sliceLength = int(rhs)
            if sliceLength == 0:
                # A sliceLength of 0 gives an empty slice
                index = slice(0, 0)
            else:
                # Slice from -sliceLength to the end
                index = slice(-sliceLength, None)
        if isinstance(lhs, Lval) and index is not None:
            return Lval(lhs, index)
        elif isinstance(lhs, PipIterable) and index is not None:
            # Use the lhs's __getitem__ with a slice argument
            if not lhs.isFinite() and sliceLength > 0:
                self.err.warn("Cannot slice from end of infinite Range")
                return nil
            else:
                return lhs[index]
        else:
            self.err.warn("Unimplemented argtypes for SUFFIX:",
                          type(lhs), "and", type(rhs))
            return nil

    def SWAP(self, lval1, lval2):
        """Exchange the values of two lvals."""
        lval1 = self.evaluate(lval1)
        lval2 = self.evaluate(lval2)
        rval1 = self.getRval(lval1)
        rval2 = self.getRval(lval2)
        if isinstance(lval1, Lval):
            self.assign(lval1, rval2)
        else:
            self.err.warn("Attempting to SWAP non-lvalue", lval1)
        if isinstance(lval2, Lval):
            self.assign(lval2, rval1)
        else:
            self.err.warn("Attempting to SWAP non-lvalue", lval2)
        return lval1

    def SWAPCASE(self, rhs):
        if isinstance(rhs, Scalar):
            return Scalar(str(rhs).swapcase())
        elif isinstance(rhs, (Range, Nil)):
            return rhs
        else:
            self.err.warn("Unimplemented argtype for SWAPCASE:", type(rhs))
            return nil

    def TANGENT(self, rhs):
        if isinstance(rhs, Scalar):
            return Scalar(math.tan(rhs.toNumber()))
        else:
            self.err.warn("Unimplemented argtype for TANGENT:", type(rhs))
            return nil

    def TITLECASE(self, rhs):
        if isinstance(rhs, Scalar):
            return Scalar(str(rhs).title())
        elif isinstance(rhs, (Range, Nil)):
            return rhs
        else:
            self.err.warn("Unimplemented argtype for TITLECASE:", type(rhs))
            return nil

    def TOBASE(self, number, base=None):
        """Convert a decimal integer to a string in the specified base."""
        if base is None:
            base = 2
        elif isinstance(base, Scalar):
            base = int(base)
        elif isinstance(base, int):
            pass
        else:
            self.err.warn("Unimplemented base type for TOBASE:",
                          type(base))
            return nil
        if base < 2 or base > len(BASE_CONVERSION_DIGITS):
            self.err.warn("Invalid base for TOBASE:", base)
            return nil
        if isinstance(number, Scalar):
            number = int(number)   # sorry, no float support
            if number == 0:
                return Scalar("0")
            elif number < 0:
                sign = "-"
                number = -number
            else:
                sign = ""
            result = ""
            while number > 0:
                result = BASE_CONVERSION_DIGITS[number % base] + result
                number //= base
            return Scalar(sign + result)
        else:
            self.err.warn("Unimplemented argtype for TOBASE:", type(number))
            return nil

    def TODIGITS(self, number, base=None):
        "Convert a decimal integer to a list of digits in the specified base."
        if isinstance(number, (List, Range)):
            return List(self.TODIGITS(num, base) for num in number)
        if base is None:
            base = 2
        elif isinstance(base, Scalar):
            base = int(base)
        elif isinstance(base, int):
            pass
        else:
            self.err.warn("Unimplemented base type for TODIGITS:",
                          type(base))
            return nil
        if base < 2:
            self.err.warn("Invalid base for conversion:", base)
            return nil
        if isinstance(number, Scalar):
            number = int(number)   # sorry, no float support
            if number < 0:
                sign = -1
                number = -number
            else:
                sign = 1
            result = []
            while number > 0:
                digit = number % base
                number //= base
                result.insert(0, Scalar(sign * digit))
            return List(result)
        else:
            self.err.warn("Unimplemented argtype for TODIGITS:", type(number))
            return nil

    def TOHEX(self, number):
        """Convert a decimal integer to a string in hexadecimal."""
        return self.TOBASE(number, 16)

    def TRANSLITERATE(self, lhs, old, new):
        """Expanded version of Python's str.translate().

        With Scalars, translate one letter to another; with Range or
        List of numbers, translate character codes.
        """
        if isinstance(lhs, (List, Range)):
            return List(self.TRANSLITERATE(item, old, new) for item in lhs)
        elif (isinstance(lhs, Scalar)
              and isinstance(old, PipIterable)
              and isinstance(new, PipIterable)):
            if not old.isFinite() and not new.isFinite():
                # Both are infinite Ranges
                self.err.warn("Cannot TRANSLITERATE one infinite "
                              "Range into another:", old, "->", new)
                return nil
            result = str(lhs)
            if isinstance(old, Scalar):
                old = str(old)
            if isinstance(new, Scalar):
                new = str(new)
            mapping = {}
            for oldChar, newChar in zip(old, new):
                if isinstance(oldChar, str):
                    oldChar = ord(oldChar)
                elif isinstance(oldChar, Scalar):
                    oldChar = oldChar.toNumber()
                else:
                    self.err.warn("Cannot TRANSLITERATE from", type(oldChar),
                                  repr(oldChar))
                    continue
                if isinstance(newChar, str):
                    newChar = ord(newChar)
                elif isinstance(newChar, Scalar):
                    newChar = newChar.toNumber()
                elif newChar is nil:
                    newChar = None
                else:
                    self.err.warn("Cannot TRANSLITERATE to", type(newChar),
                                  repr(newChar))
                    continue
                mapping[oldChar] = newChar
            result = result.translate(mapping)
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtypes for TRANSLITERATE:",
                          type(lhs), type(old), "and", type(new))
            return nil

    def TRIM(self, string, extra=None):
        if isinstance(string, Range):
            # Apply memberwise to elements of Range (a flag takes care of the
            # List case
            return List(self.TRIM(element, extra) for element in string)

        if extra is None:
            front = back = 1
        elif isinstance(extra, Scalar):
            front = back = int(extra)
        elif isinstance(extra, Range):
            front = extra.getLower() or 0
            back = extra.getUpper() or 0
        elif extra is nil:
            return string
        else:
            self.err.warn("Unimplemented argtype for rhs of TRIM:",
                          type(extra))
            return nil

        if isinstance(string, Scalar):
            front = max(front, 0)
            back = max(back, 0)
            string = str(string)
            return Scalar(string[front:len(string)-back])
        else:
            self.err.warn("Unimplemented argtype for lhs of TRIM:",
                          type(string))
            return nil

    def UNIQUE(self, iterable):
        """Remove duplicate values from iterable.

        Each value appears in the result List/Scalar in the order
        of its first appearance in the original iterable.
        """
        if isinstance(iterable, Range):
            # All values are already unique
            return iterable
        elif isinstance(iterable, Nil):
            # This is not a warning--removing duplicates from nil leaves nil
            return nil
        elif isinstance(iterable, List) or isinstance(iterable, Scalar):
            result = []
            previousItems = set()
            for item in iterable:
                # Check each item against a set of previous items; if it
                # hasn't appeared yet, add it to the result
                if item not in previousItems:
                    result.append(item)
                    previousItems.add(item)
            if isinstance(iterable, List):
                return List(result)
            elif isinstance(iterable, Scalar):
                return self.JOIN(result)
        else:
            self.err.warn("Unimplemented argtype for UNIQUE:",
                          type(iterable))
            return nil

    def UNWEAVE(self, iterable, strands=2):
        """Distribute items into multiple iterables; inverse of WEAVE."""
        if isinstance(iterable, Range):
            if iterable.isFinite():
                iterable = List(iterable)
            else:
                self.err.warn("Cannot UNWEAVE an infinite Range:", iterable)
                return nil
        if (isinstance(iterable, PipIterable)
                and isinstance(strands, (int, Scalar))):
            # Unweave the items from iterable into given number of "strands"
            # E.g. 123456789 UW 3 == [147 258 369]
            if isinstance(strands, Scalar):
                strands = int(strands)
            if strands < 1:
                self.err.warn(f"Cannot UNWEAVE into {strands} strands")
                return nil
            result = [[] for i in range(strands)]
            for index, item in enumerate(iterable):
                result[index % strands].append(item)
            if isinstance(iterable, Scalar):
                result = List(self.JOIN(item) for item in result)
            elif isinstance(iterable, List):
                result = List(List(item) for item in result)
            return result
        else:
            if strands == 2:
                # Unary version
                self.err.warn("Unimplemented argtype for UNWEAVE:",
                              type(iterable))
            else:
                # Binary version
                self.err.warn("Unimplemented argtypes for UNWEAVE:",
                              type(iterable), "and", type(strands))
            return nil

    def UPPERCASE(self, rhs):
        if isinstance(rhs, Scalar):
            return Scalar(str(rhs).upper())
        elif isinstance(rhs, (Range, Nil)):
            return rhs
        else:
            self.err.warn("Unimplemented argtype for UPPERCASE:", type(rhs))
            return nil

    def WEAVE(self, iterable1, iterable2=None):
        """Interleave two iterables, or a List of iterables."""
        if iterable2 is None:
            # Unary version: rhs is expected to be a list of iterables to be
            # woven together
            iterables = iterable1
            if isinstance(iterables, Scalar):
                # Weaving the characters just results in the same thing anyway
                return iterables
            elif isinstance(iterables, Range):
                if iterables.isFinite():
                    iterables = List(iterables)
                else:
                    self.err.warn("Cannot WEAVE infinite Range")
                    return nil
            if isinstance(iterables, List):
                result = []
                allScalar = True
                for i, iterable in enumerate(iterables):
                    if isinstance(iterable, (List, Range)):
                        allScalar = False
                        if not iterable.isFinite():
                            self.err.warn("Cannot WEAVE a List containing",
                                          "an infinite Range")
                            return nil
                    elif iterable is nil:
                        # Replace nil with empty string
                        iterables[i] = SCALAR_EMPTY
                    elif isinstance(iterable, Scalar):
                        pass
                    else:
                        self.err.warn("Cannot WEAVE a List containing",
                                      "an object of type",
                                      type(iterable))
                        return nil
                for i in range(max(map(len, iterables))):
                    for iterable in iterables:
                        if i < len(iterable):
                            result.append(iterable[i])
                if allScalar:
                    return Scalar(self.JOIN(result))
                else:
                    return List(result)
        else:
            # Binary version: two iterables to be woven together
            if iterable1 is nil:
                iterable1 = SCALAR_EMPTY
            if iterable2 is nil:
                iterable2 = SCALAR_EMPTY
            if (isinstance(iterable1, PipIterable)
                    and isinstance(iterable2, PipIterable)):
                if iterable1.isFinite() and iterable2.isFinite():
                    result = []
                    for i in range(max(len(iterable1), len(iterable2))):
                        if i < len(iterable1):
                            result.append(iterable1[i])
                        if i < len(iterable2):
                            result.append(iterable2[i])
                    if (isinstance(iterable1, Scalar)
                            and isinstance(iterable2, Scalar)):
                        return Scalar(self.JOIN(result))
                    else:
                        return List(result)
                else:
                    self.err.warn("Cannot WEAVE infinite Ranges")
                    return nil
            else:
                self.err.warn("Unimplemented argtypes for WEAVE:",
                              type(iterable1), "and", type(iterable2))
                return nil

    def WRAP(self, string, outer):
        """Prepend and append characters around string."""
        if (isinstance(string, (Scalar, Pattern))
                and isinstance(outer, (Scalar, Pattern))):
            return self.CAT(self.CAT(outer, string), outer)
        else:
            self.err.warn("Unimplemented argtypes for WRAP:",
                          type(string), "and", type(outer))
            return nil

    def YANK(self, rhs):
        """Assign rhs to the y variable."""
        lhs = Lval("y")
        self.assign(lhs, rhs)
        return lhs

    def YANKOUTPUT(self, rhs):
        """OUTPUT and then YANK rhs."""
        self.OUTPUT(rhs)
        return self.YANK(rhs)

    def YANKPRINT(self, rhs):
        """PRINT and then YANK rhs."""
        self.PRINT(rhs)
        return self.YANK(rhs)

    def ZEROGRID(self, rows, cols=None):
        if cols is None:
            cols = rows
        if isinstance(rows, Scalar) and isinstance(cols, Scalar):
            rows = range(int(rows))
            cols = range(int(cols))
            return List(List(Scalar("0") for col in cols)
                        for row in rows)
        else:
            self.err.warn("Unimplemented argtypes for ZEROGRID:",
                          type(rows), "and", type(cols))
            return nil
    
    def ZIP(self, iterable1, iterable2=None):
        if iterable2 is None:
            if isinstance(iterable1, PipIterable):
                if iterable1.isFinite():
                    iterables = iterable1
                else:
                    self.err.warn("Cannot ZIP infinite Range")
                    return nil
            else:
                self.err.warn("Cannot ZIP non-iterable:", type(iterable1))
                return nil
        else:
            iterables = [iterable1, iterable2]
        noniterables = [item
                        for item in iterables
                        if not isinstance(item, PipIterable)]
        if noniterables:
            # There are some of the "iterables" that are not iterable
            # TBD: maybe this can find a non-error meaning?
            self.err.warn("Trying to ZIP non-iterable value(s):",
                          ", ".join(str(type(item)) for item in noniterables))
            return nil
        else:
            return List(List(tuple) for tuple in zip(*iterables))
    
    def ZIPDEFAULT(self, iterables, default=None):
        if default is None:
            default = SCALAR_EMPTY
        if isinstance(iterables, PipIterable):
            if not iterables.isFinite():
                self.err.warn("Cannot ZIPDEFAULT infinite Range")
                return nil
            noniterables = [item for item in iterables
                            if not isinstance(item, PipIterable)]
            if noniterables:
                # There are some of the "iterables" that are not iterable
                # TBD: maybe this can find a non-error meaning?
                self.err.warn("Trying to ZIPDEFAULT non-iterable value(s):",
                              noniterables)
                return nil
            else:
                return List(List(tuple) for tuple in
                            itertools.zip_longest(*iterables,
                                                  fillvalue=default))
        else:
            self.err.warn("Trying to ZIPDEFAULT non-iterable:",
                          type(iterable1))
            return nil

    def ZIPJOIN(self, line1, line2=None):
        if line2 is None:
            # Unary version expects its argument to be a list of lines
            if isinstance(line1, PipIterable):
                if line1.isFinite():
                    lines = line1
                else:
                    self.err.warn("Cannot ZIPJOIN infinite Range")
                    return nil
            else:
                self.err.warn("Trying to ZIPJOIN non-iterable:", type(line1))
                return nil
        else:
            # Binary version expects its arguments to be two lines
            lines = [line1, line2]
        if isinstance(lines, Scalar):
            returnScalar = True
            lines = str(lines).split("\n")
        else:
            returnScalar = False
        # Each line should be an iterable; if it's not, cast it to Scalar
        lines = [line if isinstance(line, PipIterable) else Scalar(line)
                 for line in lines]
        result = [self.JOIN(tuple) for tuple in
                  itertools.zip_longest(*lines, fillvalue=Scalar(" "))]
        if returnScalar:
            return self.JOIN(result, Scalar("\n"))
        else:
            return List(result)


class Lval:
    def __init__(self, base, sliceValue=None):
        if isinstance(base, Lval):
            self.base = base.base
            # Make sure to copy the slicelist so changes here don't modify the
            # original
            self.sliceList = base.sliceList[:]
            if sliceValue is not None:
                self.sliceList.append(sliceValue)
            self.evaluated = base.evaluated
        else:
            if isinstance(base, List):
                self.base = base
            else:
                self.base = str(base)
            self.sliceList = []
            self.evaluated = None

    def copy(self):
        return Lval(self)

    def __str__(self):
        slices = ",".join(map(str, self.sliceList))
        if slices:
            slices = "|" + slices
        if self.evaluated is not None:
            evaluated = "=" + str(self.evaluated)
        else:
            evaluated = ""
        string = f"Lval({self.base}{evaluated}{slices})"
        return string

    def __eq__(self, rhs):
        if isinstance(rhs, Lval):
            return self.base == rhs.base and self.sliceList == rhs.sliceList
        elif isinstance(rhs, (str, List, tokens.Name)):
            return self.base == rhs and self.sliceList == []


class LocalScope:
    def __init__(self, function=None, argList=None):
        self.function = function
        self.argList = argList
        self.loopNestingLevel = -1
        self.iterationCounts = []
        self.vars = {}
        if self.argList is not None:
            for name, arg in zip("abcde", argList):
                self.vars[name] = arg
            self.vars["f"] = function
            self.vars["g"] = List(argList)

    def openLoop(self):
        self.loopNestingLevel += 1
        self.iterationCounts.append(0)
        loopVar = self.currentLoopVar()
        if loopVar is not None:
            self.vars[loopVar] = Scalar("0")

    def stepLoop(self):
        self.iterationCounts[-1] += 1
        loopVar = self.currentLoopVar()
        if loopVar is not None:
            self.vars[loopVar] = Scalar(self.iterationCounts[-1])

    def closeLoop(self):
        self.loopNestingLevel -= 1
        self.iterationCounts.pop()

    def currentLoopVar(self):
        if 0 <= self.loopNestingLevel <= 4:
            return "edcba"[self.loopNestingLevel]
        else:
            return None

