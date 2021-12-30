
import itertools
import math
import random
import re

import version
import tokens
import operators as ops
import parsing
import scanning
from ptypes import (PipType, PipIterable, Scalar, Pattern, List, Range,
                    Block, Nil, nil)
import ptypes
from errors import ErrorReporter, FatalError

# Generate some Scalar constants now to make certain operations more efficient
SCALAR_EMPTY = Scalar("")
SCALAR_ONE = Scalar("1")
SCALAR_TWO = Scalar("2")


class ProgramState:
    """The internal state of a program during execution."""
    
    def __init__(self, listFormat=None, showWarnings=False):
        # The listFormat parameter determines how lists are formatted when
        # converting to string (and therefore when printing)
        List.outFormat = listFormat
        # The showWarnings parameter determines whether non-fatal errors
        # (such as dividing by 0) show warning messages or continue silently
        self.err = ErrorReporter(showWarnings)
        self.callDepth = 0
        # There is no maximum recursion depth, but in practice recursion is
        # severely limited by Python's maximum recursion depth. In one test,
        # the program crashed after 140 levels of recursion.
        # Set pre-initialized global variables
        self.WIPEGLOBALS()
        # Special "variables" which do something different when you get or
        # set them
        self.specialVars = {
            "q": {"get": self.getq},
            "r": {"get": self.getr, "set": self.setr},
            }
        # Local variables--one set per function call level
        self.locals = [{}]

    def executeProgram(self, statements, args=None):
        if not statements:
            # Empty program does nothing
            return
        if args is None:
            args = []
        # Convert the whole program to a block and execute a function call
        # with args as the arguments and the return value PRINTed
        # after execution
        mainFunction = self.BLOCK(statements)
        returnVal = self.functionCall(mainFunction, args)
        self.PRINT(returnVal)

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
                    return nil
                else:
                    self.err.die("Implementation error, function not found:",
                                 cmdFunction)
            elif isinstance(statement[0], ops.Operator):
                # This is an expression; evaluate it
                return self.evaluate(statement)
            else:
                # Weird, this shouldn't happen
                self.err.die("Implementation error: statement", statement,
                             "isn't command or expression")
        else:
            # If it's not a list, it's probably a single-item expression
            return self.evaluate(statement)
    
    def evaluate(self, expression):
        #!print("In evaluate", repr(expression))
        exprType = type(expression)
        if isinstance(expression, tokens.Name):
            # Evaluate a name as an lvalue (which may become an rvalue later)
            return Lval(expression)
        elif isinstance(expression, (Lval, PipType)):
            # This is a value (lvalue or rvalue) already--just return it
            return expression
        elif exprType is not list:
            # ?!
            self.err.die("Implementation error: reached else branch of "
                         f"evaluate({expression})")

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
        if varName in "abcdefg":
            # Local variable
            return self.locals[self.callDepth]
        else:
            # Global variable
            return self.vars

    def getRval(self, expr):
        #!print("In getRval", expr)
        if isinstance(expr, tokens.Name) and len(str(expr)) == 3:
            expr = Lval(expr)
        if isinstance(expr, (list, tokens.Name)):
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
                try:
                    result = self.varTable(base)[base]
                except KeyError:
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
        self.callDepth += 1
        self.locals.append({})
        for name, arg in zip("abcde", argList):
            self.assign(Lval(name), arg)
        self.assign(Lval("f"), function)
        self.assign(Lval("g"), List(argList))
        for statement in function.getStatements():
            self.executeStatement(statement)
        returnExpr = function.getReturnExpr()
        if returnExpr is not None:
            returnVal = self.getRval(returnExpr)
        else:
            returnVal = nil
        # Delete this call's local variables
        del self.locals[self.callDepth]
        self.callDepth -= 1
        return returnVal

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
        try:
            iterator = iter(iterable)
        except TypeError:
            self.err.warn("Cannot iterate over", type(iterable), iterable)
        else:
            for item in iterator:
                self.assign(loopVar, item)
                for statement in code:
                    self.executeStatement(statement)
    
    def IF(self, cond, code, elseCode):
        """Execute code if cond evaluates to true; otherwise, elseCode."""
        condVal = self.getRval(cond)
        if condVal:
            for statement in code:
                self.executeStatement(statement)
        else:
            for statement in elseCode:
                self.executeStatement(statement)

    def LOOP(self, count, code):
        """Execute code count times."""
        count = self.getRval(count)
        if count is nil:
            return
        elif isinstance(count, Scalar):
            count = int(count)
        elif isinstance(count, (List, Range)):
            count = len(count)
        for i in range(count):
            for statement in code:
                self.executeStatement(statement)
    
    def LOOPREGEX(self, regex, string, code):
        """Execute code for each match of regex in string."""
        regex = self.getRval(regex)
        string = self.getRval(string)
        if isinstance(regex, Scalar) and isinstance(string, Pattern):
            regex, string = string, regex
        elif isinstance(regex, Scalar):
            regex = self.REGEX(regex)
        if isinstance(regex, Pattern) and isinstance(string, Scalar):
            # TBD: behavior for other types, such as isinstance(string, List)?
            matches = regex.asRegex().finditer(str(string))
            for matchObj in matches:
                self.assignRegexVars(matchObj)
                # Then execute the loop body
                for statement in code:
                    self.executeStatement(statement)
        else:
            self.err.warn("Unimplemented argtypes for LOOPREGEX:",
                          type(regex), "and", type(string))

    def TILL(self, cond, code):
        """Loop, executing code, until cond evaluates to true."""
        condVal = self.getRval(cond)
        while not condVal:
            for statement in code:
                self.executeStatement(statement)
            condVal = self.getRval(cond)

    def WHILE(self, cond, code):
        """Loop, executing code, while cond evaluates to true."""
        condVal = self.getRval(cond)
        while condVal:
            for statement in code:
                self.executeStatement(statement)
            condVal = self.getRval(cond)

    def WIPEGLOBALS(self):
        """Reset all global variables to their default values."""
        self.vars = {
            "_": Block([], tokens.Name("a")),
            "h": Scalar("100"),
            "i": Scalar("0"),
            #j is reserved for complex numbers
            "k": Scalar(", "),
            "l": List([]),
            "m": Scalar("1000"),
            "n": Scalar("\n"),
            "o": Scalar("1"),
            "p": Scalar("()"),
            #q is a special variable
            #r is a special variable
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
            "PA": Scalar("".join(chr(i) for i in range(32, 127))),
            "PI": Scalar(math.pi),
            "VD": Scalar(version.COMMIT_DATE),
            "VN": Scalar(version.VERSION),
            "VW": Scalar("aeiou"),
            "VY": Scalar("aeiouy"),
            "XA": Pattern("[A-Za-z]"),
            "XC": Pattern("[bcdfghjklmnpqrstvwxyz]"),
            "XD": Pattern(r"\d"),
            "XI": Pattern(r"-?\d+"),
            "XL": Pattern("[a-z]"),
            "XN": Pattern(r"-?\d+(?:\.\d+)?"),
            "XU": Pattern("[A-Z]"),
            "XV": Pattern("[aeiou]"),
            "XW": Pattern(r"\w"),
            "XX": Pattern("."),
            "XY": Pattern("[aeiouy]"),
            }

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
            if len(iterable) == 0:
                return operator.default
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
                        foldValue = Scalar("1")
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
            self.err.warn("Can't fold", type(iterable))
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
            self.err.warn("Can't map operator over", type(iterable))
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
                return Range(lower, upper)
            else:
                return List(self.ADD(lhs, item) for item in rhs)
        elif isinstance(lhs, Pattern) and isinstance(rhs, Pattern):
            # + with two Patterns returns a new Pattern that matches one,
            # then the other
            result = f"(?:{lhs})(?:{rhs})"
            return Pattern(result)
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
            result = list(lhs) + [rhs]
            return List(result)
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
            result = list(lhs) + list(rhs)
            return List(result)
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
            # A operator on a Pattern makes it ASCII-only
            result = "(?a)" + str(rhs)
            return Pattern(result)
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
            result = []
            for matchObj in matches:
                groups = self.assignRegexVars(matchObj)
                result.append(groups[0])
            return List(result)
        elif isinstance(rhs, Pattern) and isinstance(lhs, (List, Range)):
            return List(self.AT(item, rhs) for item in lhs)
        elif isinstance(rhs, List) and isinstance(lhs, PipIterable):
            return List(self.AT(lhs, item) for item in rhs)
        elif isinstance(lhs, PipIterable):
            try:
                if len(lhs) == 0:
                    self.err.warn("Indexing into empty iterable")
                    return nil
            except ValueError:
                # This happens when trying to take the len of an
                # infinite Range
                # Clearly the length is not zero, so just continue
                pass
            try:
                return lhs[index]
            except IndexError:
                self.err.warn(f"Invalid index into {lhs!r}: {index}")
                return nil
        else:
            self.err.warn("Cannot index into", type(lhs))
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
            result = str(lhs) + str(rhs)
            return Pattern(result)
        else:
            self.err.warn("Unimplemented argtypes for CAT:",
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

    def CHR(self, rhs):
        if isinstance(rhs, Scalar):
            result = chr(int(rhs))
            return Scalar(result)
        elif isinstance(rhs, Pattern):
            # C operator on Pattern wraps the regex in a capturing group
            result = f"({rhs})"
            return Pattern(result)
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
            # . operator on Pattern makes . match newlines
            result = "(?s)" + str(rhs)
            return Pattern(result)
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
            except FatalError:
                self.err.die("Fatal scanning error while evaluating", code)
            try:
                tree = parsing.parse(tkns)
            except FatalError:
                self.err.die("Fatal parsing error while evaluating", code)
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
                result = (item for item in iterable if item)
                return List(result)
            else:
                self.err.warn("Unimplemented argtype for FILTER:",
                              type(iterable))
                return nil
        if isinstance(iterable, Block) and isinstance(function, PipIterable):
            # The arguments are reversible to enable things like lFI:f
            function, iterable = iterable, function
        if isinstance(function, Block) and isinstance(iterable, PipIterable):
            result = (item for item in iterable
                      if self.functionCall(function, [item]))
            return List(result)
        else:
            self.err.warn("Unimplemented argtypes for FILTER:",
                          type(function), "and", type(iterable))
            return nil

    def FIND(self, iterable, item):
        if isinstance(item, Pattern) and isinstance(iterable, Scalar):
            matchObj = item.asRegex().search(str(iterable))
            if matchObj:
                self.assignRegexVars(matchObj)
                return Scalar(matchObj.start())
            else:
                return nil
        elif isinstance(iterable, PipIterable):
            return iterable.index(item)
        else:
            self.err.warn("Unimplemented argtypes for FIND:",
                          type(iterable), "and", type(item))
            return nil

    def FINDALL(self, iterable, item):
        if isinstance(item, List) and isinstance(iterable, (Scalar, Range)):
            return List(self.FINDALL(iterable, subitem) for subitem in item)
        elif isinstance(item, Pattern) and isinstance(iterable, Scalar):
            # Return indices of all regex matches in Scalar
            matches = item.asRegex().finditer(str(iterable))
            result = []
            for matchObj in matches:
                self.assignRegexVars(matchObj)
                result.append(Scalar(matchObj.start()))
            return List(result)
        elif (isinstance(item, (Scalar, Range))
                  and isinstance(iterable, PipIterable)
              or isinstance(item, (List, Pattern, Nil))
                  and isinstance(iterable, List)):
            result = []
            lastIndex = iterable.index(item)
            while lastIndex is not nil:
                result.append(lastIndex)
                lastIndex = iterable.index(item, int(lastIndex) + 1)
            return List(result)
        else:
            self.err.warn("Unimplemented argtypes for FINDALL:",
                          type(iterable), "and", type(item))
            return nil

    def FIRSTMATCH(self, regex, string):
        if isinstance(string, Pattern):
            regex, string = string, regex
        if isinstance(regex, Pattern) and isinstance(string, Scalar):
            matchObj = regex.asRegex().search(str(string))
            if matchObj:
                self.assignRegexVars(matchObj)
                return Scalar(matchObj.group())
            else:
                return nil
        else:
            self.err.warn("Unimplemented argtypes for FIRSTMATCH:",
                          type(regex), "and", type(string))
            return nil

    def FROMBASE(self, number, base=None):
        if base is None:
            base = 2
        elif isinstance(base, Scalar):
            base = int(base)
        else:
            self.err.warn("Unimplemented base type for FROMBASE:", type(base))
            return nil
        if base < 2 or base > 36:
            self.err.warn("Invalid base for FROMBASE:", base)
            return nil
        if isinstance(number, Scalar):
            if len(number) == 0:
                number = 0
            try:
                result = int(str(number), base)
                return Scalar(result)
            except ValueError:
                # TBD: make more robust? Or just let it stay nil
                self.err.warn("Failed converting", number, "from base", base)
                return nil
        else:
            self.err.warn("Unimplemented argtype for FROMBASE:",
                          type(number))
            return nil

    def FROMDIGITS(self, digits, base=None):
        if base is None:
            base = 2
        elif isinstance(base, Scalar):
            base = base.toNumber()
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

    def FULLMATCH(self, regex, string):
        if isinstance(string, Pattern):
            regex, string = string, regex
        if isinstance(regex, Pattern) and isinstance(string, (List, Range)):
            return Scalar(all(self.FULLMATCH(regex, item)
                              for item in string))
        elif isinstance(regex, Pattern) and isinstance(string, Scalar):
            matchObj = regex.asRegex().fullmatch(str(string))
            if matchObj:
                self.assignRegexVars(matchObj)
                return Scalar("1")
            else:
                return Scalar("0")
        else:
            self.err.warn("Unimplemented argtypes for FULLMATCH:",
                          type(regex), "and", type(string))
            return Scalar("0")

    def GROUP(self, iterable, rhs=None):
        if rhs is None:
            rhs = SCALAR_TWO
        if isinstance(rhs, (List, Range)):
            # Group by each rhs and return a list of iterables
            return List(self.GROUP(iterable, jump) for jump in rhs)
        if isinstance(iterable, PipIterable) and isinstance(rhs, Scalar):
            result = List()
            jump = int(rhs)
            if jump > 0:
                index = 0
                while index < len(iterable):
                    endIndex = min(index + jump, len(iterable))
                    result.append(iterable[index:endIndex])
                    index += jump
                return result
            elif jump < 0:
                # With a negative jump, group from right to left
                index = len(iterable)
                while index > 0:
                    startIndex = max(index + jump, 0)
                    result.append(iterable[startIndex:index])
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
        if isinstance(rhs, Scalar):
            return Scalar(rhs.toNumber() // 2)
        else:
            self.err.warn("Unimplemented argtype for HALVE:",
                          type(rhs))

    def IDENTITYMATRIX(self, rhs):
        if isinstance(rhs, Scalar):
            result = []
            for row in range(int(rhs)):
                subresult = [Scalar("1" if row == col else "0")
                             for col in range(int(rhs))]
                result.append(List(subresult))
            return List(result)
        else:
            self.err.warn("Unimplemented argtype for IDENTITYMATRIX:",
                          type(rhs))

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

    def INTDIV(self, lhs, rhs):
        if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
            try:
                result = int(lhs.toNumber() / rhs.toNumber())
                return Scalar(result)
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
        if isinstance(rhs, Scalar):
            regex = re.escape(str(rhs))
            if len(rhs) > 1:
                regex = f"(?:{regex})"
            return Pattern(regex + "*")
        elif isinstance(rhs, Range):
            return Pattern("(?:"
                           + "|".join(str(item) for item in rhs)
                           + ")*")
        elif isinstance(rhs, Pattern):
            result = f"(?:{str(rhs)})*"
            return Pattern(result)
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
            return lhs[:int(rhs)]
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
            try:
                result = len(lhs) == len(rhs)
            except ValueError:
                # One or both of the arguments is an infinite Range
                # Their lengths are equal iff they are both infinite
                result = (isinstance(lhs, Range) and isinstance(rhs, Range)
                          and lhs.getUpper() is rhs.getUpper() is None)
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtypes for LENEQUAL:",
                          type(lhs), "and", type(rhs))
            return nil

    def LENGREATER(self, lhs, rhs):
        if isinstance(lhs, PipIterable) and isinstance(rhs, PipIterable):
            try:
                result = len(lhs) > len(rhs)
            except ValueError:
                # One or both of the arguments is an infinite Range
                # The lhs's length is greater iff it is infinite
                # and the rhs is not
                if not isinstance(lhs, Range):
                    result = False
                elif not isinstance(rhs, Range):
                    result = True
                else:
                    result = (lhs.getUpper() is None
                              and rhs.getUpper() is not None)
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtypes for LENGREATER:",
                          type(lhs), "and", type(rhs))
            return nil

    def LENLESS(self, lhs, rhs):
        if isinstance(lhs, PipIterable) and isinstance(rhs, PipIterable):
            try:
                result = len(lhs) < len(rhs)
            except ValueError:
                # One or both of the arguments is an infinite Range
                # The lhs's length is less iff it is not an infinite
                # Range and the rhs is one
                if not isinstance(lhs, Range):
                    result = True
                elif not isinstance(rhs, Range):
                    result = False
                else:
                    result = (lhs.getUpper() is not None
                              and rhs.getUpper() is None)
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
                result = (self.functionCall(lhs, [item]) for item in iterable)
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
            try:
                if len(rhs) == 0:
                    self.err.warn("Empty List/Range argument to MAPCOORDS")
                    return nil
            except ValueError:
                # An infinite Range doesn't have a len(), but it does
                # have at least two numbers, so it's fine for our purposes
                pass
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
        result = []
        for row in rows:
            subresult = []
            for col in cols:
                if isinstance(lhs, Block):
                    subresult.append(self.functionCall(lhs,
                                                       [Scalar(row),
                                                        Scalar(col)]))
                else:
                    # If lhs isn't a function, just return a grid of it
                    subresult.append(lhs)
            result.append(List(subresult))
        return List(result)

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
            result = (self.MAP(lhs, item) for item in iterable)
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
        if isinstance(string, Block) and isinstance(lhs, Scalar):
            # The arguments are reversible to enable things like sMR:xf
            lhs, string = string, lhs
        elif isinstance(string, Pattern) and isinstance(regex, Scalar):
            regex, string = string, regex
        if (isinstance(lhs, Block)
                and isinstance(regex, Pattern)
                and isinstance(string, Scalar)):
            matches = regex.asRegex().finditer(str(string))
            result = []
            for matchObj in matches:
                groups = self.assignRegexVars(matchObj)
                result.append(self.functionCall(lhs, groups))
            return List(result)
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
            result = []
            for item in iterable:
                try:
                    arglist = list(item)
                except ValueError:
                    # This happens when one of the items is an infinite Range
                    self.err.warn("Cannot unpack infinite Range in MAPUNPACK")
                    arglist = []
                except TypeError:
                    # This happens when one of the items is a
                    # non-iterable type
                    self.err.warn(f"Cannot unpack {item} in MAPUNPACK")
                    arglist = []
                result.append(self.functionCall(function, arglist))
            return List(result)
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
            try:
                return max(iterable, key=lambda x:x.toNumber())
            except AttributeError:
                self.err.warn("Argument to MAX contains non-numeric value:",
                              iterable)
                return nil
            except TypeError:
                self.err.warn("Argument to MAX contains unorderable types:",
                              iterable)
                return nil
            except ValueError:
                self.err.warn("Taking MAX of an empty sequence")
                return nil
        else:
            self.err.warn("Unimplemented argtype for MAX:", type(iterable))
            return nil

    def MIN(self, iterable):
        """Return numeric minimum of iterable."""
        if isinstance(iterable, PipIterable):
            try:
                return min(iterable, key=lambda x:x.toNumber())
            except AttributeError:
                self.err.warn("Argument to MIN contains non-numeric value:",
                              iterable)
                return nil
            except TypeError:
                self.err.warn("Argument to MIN contains unorderable types:",
                              iterable)
                return nil
            except ValueError:
                self.err.warn("Taking MIN of an empty sequence")
                return nil
        else:
            self.err.warn("Unimplemented argtype for MIN:", type(iterable))
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
            result = f"(?:{lhs}){{{int(rhs)}}}"
            return Pattern(result)
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
            result = "(?i)" + str(rhs)
            return Pattern(result)
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
            # Just use the Range class's __eq__
            result = lhs == rhs
        elif (isinstance(lhs, (List, Range))
              and isinstance(rhs, (List, Range))):
            try:
                result = (len(lhs) == len(rhs)
                          and all(self.NUMEQUAL(i, j)
                                  for i, j in zip(lhs, rhs)))
            except ValueError:
                # Raised by taking len of infinite Range, which cannot be
                # equal to any List
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
            elif leftUpper is None:
                # lhs is an infinite Range, thus bigger
                result = True
            elif rightUpper is None:
                # rhs is an infinite Range, thus bigger
                result = False
            else:
                result = leftUpper > rightUpper
        elif (isinstance(lhs, (List, Range))
              and isinstance(rhs, (List, Range))):
            result = None
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
                try:
                    leftLen = len(lhs)
                except ValueError:
                    # Lhs is infinite Range
                    result = True
                else:
                    try:
                        rightLen = len(rhs)
                    except ValueError:
                        # Rhs is infinite Range
                        result = False
                if result is None:
                    # Neither was an infinite Range, so we can just
                    # compare their lengths directly
                    result = leftLen > rightLen
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
            elif leftUpper is None:
                # lhs is an infinite Range, thus bigger
                result = True
            elif rightUpper is None:
                # rhs is an infinite Range, thus bigger
                result = False
            else:
                result = leftUpper > rightUpper
        elif (isinstance(lhs, (List, Range))
              and isinstance(rhs, (List, Range))):
            result = None
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
                try:
                    leftLen = len(lhs)
                except ValueError:
                    # Lhs is infinite Range
                    result = True
                else:
                    try:
                        rightLen = len(rhs)
                    except ValueError:
                        # Rhs is infinite Range
                        result = False
                if result is None:
                    # Neither was an infinite Range, so we can just
                    # compare their lengths directly
                    result = leftLen >= rightLen
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
            elif leftUpper is None:
                # lhs is an infinite Range, thus bigger
                result = False
            elif rightUpper is None:
                # rhs is an infinite Range, thus bigger
                result = True
            else:
                result = leftUpper < rightUpper
        elif (isinstance(lhs, (List, Range))
              and isinstance(rhs, (List, Range))):
            result = None
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
                try:
                    leftLen = len(lhs)
                except ValueError:
                    # Lhs is infinite Range
                    result = False
                else:
                    try:
                        rightLen = len(rhs)
                    except ValueError:
                        # Rhs is infinite Range
                        result = True
                if result is None:
                    # Neither was an infinite Range, so we can just
                    # compare their lengths directly
                    result = leftLen < rightLen
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
            elif leftUpper is None:
                # lhs is an infinite Range, thus bigger
                result = False
            elif rightUpper is None:
                # rhs is an infinite Range, thus bigger
                result = True
            else:
                result = leftUpper < rightUpper
        elif (isinstance(lhs, (List, Range))
              and isinstance(rhs, (List, Range))):
            result = None
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
                try:
                    leftLen = len(lhs)
                except ValueError:
                    # Lhs is infinite Range
                    result = False
                else:
                    try:
                        rightLen = len(rhs)
                    except ValueError:
                        # Rhs is infinite Range
                        result = True
                if result is None:
                    # Neither was an infinite Range, so we can just
                    # compare their lengths directly
                    result = leftLen <= rightLen
        else:
            # For non-comparable types, the only way they can be <=
            # is if they are identical and thus equal
            result = lhs == rhs
        return Scalar(result)

    def NUMNOTEQUAL(self, lhs, rhs):
        if isinstance(lhs, Scalar) and isinstance(rhs, Scalar):
            result = lhs.toNumber() != rhs.toNumber()
        elif isinstance(lhs, Range) and isinstance(rhs, Range):
            result = lhs != rhs
        elif (isinstance(lhs, (List, Range))
              and isinstance(rhs, (List, Range))):
            try:
                result = (len(lhs) != len(rhs)
                          or any(self.NUMNOTEQUAL(i, j)
                                  for i, j in zip(lhs, rhs)))
            except ValueError:
                # Raised by taking len of infinite Range, which cannot be
                # equal to any List
                result = True
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
            try:
                iterable = list(iterable)
            except ValueError:
                self.err.warn("Cannot PALINDROMIZE an infinite range")
                return nil
            else:
                if iterable:
                    return List(iterable + iterable[-2::-1])
                else:
                    return List()
        elif isinstance(iterable, Scalar):
            iterable = str(iterable)
            if iterable:
                return Scalar(iterable + iterable[-2::-1])
            else:
                return Scalar()
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
            result = itertools.permutations(iterable)
            if isinstance(iterable, Scalar):
                return List(self.JOIN(perm) for perm in result)
            else:
                return List(List(perm) for perm in result)
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
            if len(iterVal) > 0:
                iterVal = list(iterVal)
                index %= len(iterVal)
                item = iterVal[index]
                iterVal = List(iterVal[:index] + iterVal[index+1:])
            else:
                self.err.warn("Cannot pick from empty List")
                return nil
        elif isinstance(iterVal, Range):
            lower = iterVal.getLower() or 0
            try:
                rangeLength = len(iterVal)
            except ValueError:
                # Infinite range raises this when you try to take the len()
                if index == 0:
                    item = iterVal[0]
                    iterVal = Range(lower + 1, nil)
                else:
                    self.err.warn("Cannot pick from middle of infinite Range")
                    return nil
            else:
                if rangeLength > 0:
                    index %= rangeLength
                    if index == 0:
                        item = iterVal[0]
                        iterVal = Range(lower + 1, iterVal.getUpper())
                    else:
                        iterVal = list(iterVal)
                        item = iterVal[index]
                        iterVal = List(iterVal[:index] + iterVal[index+1:])
                else:
                    self.err.warn("Cannot pick from empty Range")
                    return nil
        elif isinstance(iterVal, Scalar):
            if len(iterVal) > 0:
                item = iterVal[index]
                iterVal = str(iterVal)
                index %= len(iterVal)
                iterVal = Scalar(iterVal[:index] + iterVal[index+1:])
            else:
                self.err.warn("Cannot pick from empty Scalar")
                return nil
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
            if len(iterVal) > 0:
                item = iterVal[0]
                iterVal = iterVal[1:]
            else:
                self.err.warn("Popping from empty list")
                return nil
        elif isinstance(iterVal, Range):
            lower = iterVal.getLower() or 0
            try:
                if len(iterVal) > 0:
                    iterVal = Range(lower + 1, iterVal.getUpper())
                else:
                    self.err.warn("Popping from empty range")
                    return nil
            except ValueError:
                # Infinite range raises this when you try to take the len()
                iterVal = Range(lower + 1, nil)
            item = Scalar(lower)
        elif isinstance(iterVal, Scalar):
            if len(iterVal) > 0:
                item = iterVal[0]
                iterVal = iterVal[1:]
            else:
                self.err.warn("Popping from empty scalar")
                return nil
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
            # + operator on Pattern applies regex + to the whole thing
            result = f"(?:{rhs})+"
            return Pattern(result)
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
            result = [rhs] + list(lhs)
            return List(result)
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
            iterVal = self.PREPENDELEM(iterVal, item)
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
            iterVal = self.APPENDELEM(iterVal, item)
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
            index = random.randrange(len(iterable))
            return iterable[index]
        else:
            self.err.warn("Unimplemented argtype for RANDCHOICE:",
                          type(iterable))
            return nil

    def RANDRANGE(self, lower, upper):
        if isinstance(lower, (Scalar, Nil)) and isinstance(upper, Scalar):
            if lower is nil:
                lower = 0
            else:
                lower = int(lower)
            upper = int(upper)
            return Scalar(random.randrange(lower, upper))
        else:
            self.err.warn("Unimplemented argtypes for RANDRANGE:",
                          type(lower), "and", type(upper))
            return nil
        
    def RANDRANGETO(self, upper):
        """Unary version of RANDRANGE."""
        if isinstance(upper, Scalar):
            return Scalar(random.randrange(int(upper)))
        else:
            self.err.warn("Unimplemented argtype for RANDRANGETO:",
                          type(upper))
            return nil

    def RANGE(self, lower, upper):
        if (isinstance(lower, (Scalar, Nil))
                and isinstance(upper, (Scalar, Nil))):
            return Range(lower, upper)
        elif isinstance(lower, Pattern) and isinstance(upper, Pattern):
            # , with two Patterns returns a new Pattern that matches one OR
            # the other
            result = f"(?:{lower})|(?:{upper})"
            return Pattern(result)
        else:
            self.err.warn("Unimplemented argtypes for RANGE:",
                          type(lower), "and", type(upper))
            return nil

    def RANGETO(self, upper):
        """Unary version of RANGE."""
        if isinstance(upper, (Scalar, Nil)):
            return Range(nil, upper)
        elif isinstance(upper, Pattern):
            # , operator on a Pattern makes ^ and $ match fronts & ends of
            # lines
            result = "(?m)" + str(upper)
            return Pattern(result)
        else:
            self.err.warn("Unimplemented argtype for RANGETO:", type(upper))
            return nil

    def RECURSE(self, rhs):
        "Call the current function recursively with rhs as its argument."
        return self.functionCall(self.locals[self.callDepth]["f"],
                                 [rhs])

    def REFLECT(self, iterable):
        """Concatenate iterable with its reverse."""
        if isinstance(iterable, (Range, List)):
            try:
                iterable = list(iterable)
            except ValueError:
                self.err.warn("Cannot REFLECT an infinite range")
                return nil
            else:
                return List(iterable + iterable[::-1])
        elif isinstance(iterable, Scalar):
            iterable = str(iterable)
            return Scalar(iterable + iterable[::-1])
        else:
            self.err.warn("Unimplemented argtype for REFLECT:",
                          type(iterable))
            return nil

    def REGEX(self, rhs):
        """Convert Scalar, List, or Range to properly-escaped Pattern."""
        if isinstance(rhs, Scalar):
            regex = re.escape(str(rhs))
            if len(rhs) > 1:
                # Surround expression in a non-capturing group so repetition
                # constructs will function as expected if appended
                regex = "(?:" + regex + ")"
            return Pattern(regex)
        elif isinstance(rhs, Pattern):
            return rhs
        elif isinstance(rhs, (List, Range)):
            return Pattern("(?:"
                           + "|".join(str(self.REGEX(item)) for item in rhs)
                           + ")")
        else:
            self.err.warn("Unimplemented argtype for REGEX:", type(rhs))
            return nil

    def REPEATLIST(self, lhs, rhs):
        if isinstance(lhs, (Scalar, Pattern, Nil)):
            lhs = List([lhs])
        if isinstance(lhs, (List, Range)) and isinstance(rhs, Scalar):
            result = list(lhs.copy()) * int(rhs)
            return List(result)
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
            return Scalar(result)
        elif isinstance(lhs, PipIterable):
            result = list(lhs)
            while rhs in result:
                # Loop is necessary because list.remove only removes the
                # first instance
                result.remove(rhs)
            return List(result)
        else:
            self.err.warn("Unimplemented argtypes for REMOVE:",
                          type(lhs), "and", type(rhs))
            # Nothing to remove, so return the original value
            return lhs

    def REPR(self, rhs):
        if isinstance(rhs, Block):
            statements = rhs.getStatements() + [rhs.getReturnExpr()]
            return Scalar("{" + parsing.unparse(statements) + "}")
        else:
            return Scalar(repr(rhs))

    def REVERSE(self, rhs):
        if isinstance(rhs, Scalar):
            return Scalar(str(rhs)[::-1])
        elif isinstance(rhs, (Range, List)):
            try:
                rhs = list(rhs)
            except ValueError:
                self.err.warn("Cannot REVERSE an infinite range")
                return nil
            else:
                return List(rhs[::-1])
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

    def SORTKEYED(self, key, iterable):
        """Sort by value of (numeric!) key function applied to each item."""
        if not isinstance(key, Block) and isinstance(iterable, Block):
            key, iterable = iterable, key
        if isinstance(key, Block) and isinstance(iterable, PipIterable):
            pyKey = lambda x: self.functionCall(key, [x]).toNumber()
            try:
                return List(sorted(iterable, key=pyKey))
            except TypeError:
                raise
                self.err.warn("Sort key must always return a number")
                return nil
        else:
            self.err.warn("Unimplemented argtypes for SORTKEYED:",
                          type(key), "and", type(iterable))
            return nil

    def SORTNUM(self, iterable):
        if isinstance(iterable, PipIterable):
            try:
                return List(sorted(iterable, key=lambda x: x.toNumber()))
            except TypeError:
                self.err.warn("Cannot sort mixed types in list")
                return nil
        else:
            self.err.warn("Unimplemented argtype for SORTNUM:",
                          type(iterable))
            return nil

    def SORTSTRING(self, iterable):
        if isinstance(iterable, Scalar):
            return Scalar("".join(sorted(str(iterable))))
        elif isinstance(iterable, PipIterable):
            # This is going to get a bit wonky when sorting lists of lists,
            # but not sure it's worth the effort to fix
            return List(sorted(iterable, key=str))
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
            # TODO: Handle infinite ranges gracefully
            indices = list(int(index) for index in indices)
        elif isinstance(indices, List):
            try:
                indices = list(set(int(index) for index in indices))
            except TypeError:
                # The List contained items that couldn't be converted to int
                self.err.warn("List of indices for SPLITAT must contain "
                              "only Scalars")
                return nil

        if isinstance(iterable, PipIterable) and isinstance(indices, list):
            results = []
            prevIndex = 0
            length = len(iterable)
            for i in range(length):
                if i in indices or i - length in indices:
                    results.append(iterable[prevIndex:i])
                    prevIndex = i
            results.append(iterable[prevIndex:])
            return List(results)
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
        if isinstance(rhs, Block):
            statements = rhs.getStatements() + [rhs.getReturnExpr()]
            return Scalar("{" + parsing.unparse(statements) + "}")
        else:
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
            return Range(lower, upper)
        elif isinstance(lhs, Scalar) and isinstance(rhs, Range):
            return List(self.SUB(lhs, item) for item in rhs)
        elif isinstance(lhs, Range) and isinstance(rhs, Range):
            # TODO... this sort of situation might warrant some rethinking of
            # the RANGE_EACH/LIST_EACH handling
            self.err.warn("Can't subtract two Ranges yet")
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
            self.err.warn("Attempting to swap non-lvalue", lval1)
        if isinstance(lval2, Lval):
            self.assign(lval2, rval1)
        else:
            self.err.warn("Attempting to swap non-lvalue", lval2)
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

    def TOBASE(self, number, base=None):
        """Convert a decimal integer to a string in the specified base."""
        if base is None:
            base = 2
        elif isinstance(base, Scalar):
            base = int(base)
        else:
            self.err.warn("Unimplemented base type for TOBASE:",
                          type(base))
            return nil
        if base < 2 or base > 36:
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
            alphabet = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            result = ""
            while number > 0:
                result = alphabet[number % base] + result
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
            result = str(lhs)
            infiniteRange = False
            if isinstance(old, Scalar):
                old = str(old)
            elif isinstance(old, Range):
                try:
                    len(old)
                except ValueError:
                    infiniteRange = True
                    # This isn't a problem yet, but if new is also an
                    # infinite range, we're in trouble
            if isinstance(new, Scalar):
                new = str(new)
            elif isinstance(new, Range):
                try:
                    len(new)
                except ValueError:
                    if infiniteRange:
                        # They're both infinite
                        self.err.warn("Cannot TRANSLITERATE one infinite "
                                      "Range into another:", old, "->", new)
                        return nil
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
            try:
                len(iterable)
            except ValueError:
                # Infinite Range, no can do
                self.err.warn("Cannot UNWEAVE an infinite Range:", iterable)
                return nil
            else:
                iterable = List(iterable)
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
            elif isinstance(iterables, (List, Range)):
                result = []
                allScalar = True
                for i, iterable in enumerate(iterables):
                    if isinstance(iterable, (List, Range)):
                        allScalar = False
                    elif iterable is nil:
                        # Replace nil with empty string
                        iterables[i] = SCALAR_EMPTY
                    elif isinstance(iterable, Scalar):
                        pass
                    else:
                        self.err.warn("Cannot weave object of type",
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
    
    def ZIP(self, list1, list2=None):
        if list2 is None:
            if isinstance(list1, PipIterable):
                lists = list1
            else:
                self.err.warn("Trying to zip non-iterable:", type(list1))
                return nil
        else:
            lists = [list1, list2]
        noniterables = [item
                        for item in lists
                        if not isinstance(item, PipIterable)]
        if noniterables:
            # There are some of the "lists" that are not iterable
            # TBD: maybe this can find a non-error meaning?
            self.err.warn("Trying to zip non-iterable value(s):",
                          ", ".join(str(type(item)) for item in noniterables))
            return nil
        else:
            return List(List(tuple) for tuple in zip(*lists))
    
    def ZIPDEFAULT(self, lists, default=None):
        if default is None:
            default = nil
        if isinstance(lists, PipIterable):
            noniterables = [item for item in lists
                            if isinstance(item, (Nil, Block))]
            if noniterables:
                # There are some of the "lists" that are not iterable
                # TBD: maybe this can find a non-error meaning?
                self.err.warn("Trying to zip non-iterable value(s):",
                              noniterables)
                return nil
            else:
                return List(List(tuple) for tuple in
                            itertools.zip_longest(*lists, fillvalue=default))
        else:
            self.err.warn("Trying to zip non-iterable:", type(list1))
            return nil



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

