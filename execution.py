
import math, random, itertools
import tokens
import operators as ops
from parsing import isExpr
from ptypes import Scalar, Pattern, List, Range, Block, Nil, nil
from errors import ErrorReporter

# Generate a Scalar constant 1 now to make (in|de)crements more efficient
scalarOne = Scalar(1)


class ProgramState:
    """Represents the internal state of a program during execution."""
    
    def __init__(self, listFormat=None, showWarnings=False):
        # The listFormat parameter determines how lists are formatted when
        # converting to string (and therefore when printing)
        List.outFormat = listFormat
        # The showWarnings parameter determines whether non-fatal errors
        # (such as dividing by 0) show warning messages or continue silently
        self.err = ErrorReporter(showWarnings)
        self.callDepth = -1
        # There is no maximum recursion depth, but in practice recursion is
        # severely limited by Python's maximum recursion depth. In one test,
        # the program crashed after 140 levels of recursion.
        # Pre-initialized global variables
        self.vars = {
            "_": Block([], tokens.Name("a")),
            "h": Scalar("100"),
            "i": Scalar("0"),
            "l": List([]),
            "m": Scalar("1000"),
            "n": Scalar("\n"),
            "o": Scalar("1"),
            "s": Scalar(" "),
            "t": Scalar("10"),
            "u": nil,
            "v": Scalar("-1"),
            "x": Scalar(""),
            "y": Scalar(""),
            "z": Scalar(""),
            "AZ": Scalar("ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
            "PA": Scalar("".join(chr(i) for i in range(32, 127))),
            "PI": Scalar(math.pi),
            }
        # Special "variables" which do something different when you get or
        # set them
        self.specialVars = {
            "q": {"get": self.getq},
            "r": {"get": self.getr, "set": self.setr},
            }
        # Local variables--one set per function call level
        self.locals = []

    def executeProgram(self, statements, cmdLineArgs=None):
        if not statements:
            # Empty program does nothing
            return
        if cmdLineArgs is None:
            cmdLineArgs = []
        else:
            cmdLineArgs = [Scalar(arg) for arg in cmdLineArgs]
        # Convert the whole program to a block and execute a function call
        # with cmdLineArgs as the arguments and the return value PRINTed
        # after execution
        mainFunction = self.BLOCK(statements)
        returnVal = self.functionCall(mainFunction, cmdLineArgs)
        self.PRINT(returnVal)

    def executeStatement(self, statement):
        if type(statement) is list:
            if type(statement[0]) == ops.Command:
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
            elif type(statement[0]) == ops.Operator:
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
        if exprType is tokens.Name:
            # Evaluate a name as an lvalue (which may become an rvalue later)
            return Lval(expression)
        elif exprType in (Lval, Scalar, Pattern, List, Range, Block, Nil):
            # This is a value (lvalue or rvalue) already--just return it
            return expression
        elif exprType is not list:
            # ?!
            self.err.die("Implementation error:",
                         "reached else branch of evaluate(%s)" % expression)

        # If none of the above were true, then we're dealing with a parse tree
        # in the form of a list: [operator, arg1, arg2, ...]
        operator, *args = expression
        fnName = operator.function
        if fnName not in dir(self):
            self.err.die("Implementation error, op function not found:",
                         fnName)
        #!print("evaluating", fnName)
        opFunction = getattr(self, fnName)
        
        if operator.assign:
            # This is a compute-and-assign operator like +:
            # Compute the expression, and then assign it back to the lval
            lval = self.evaluate(args[0])
            normalOp = operator.copy()
            normalOp.assign = False
            result = self.evaluate([normalOp] + args)
            result = self.ASSIGN(lval, result)
        elif operator.fold:
            # A binary operator being used in a unary fold operation
            result = self.FOLD(operator, *args)
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
                    if operator.flags & ops.RANGE_EACH and type(arg) is Range:
                        argsToExpand.append(i)
                    elif operator.flags & ops.LIST_EACH and type(arg) is List:
                        argsToExpand.append(i)
                    elif (operator.flags & ops.IN_LAMBDA
                          and (type(arg) is Block
                               or type(arg) is Lval
                               and type(self.getRval(arg)) is Block)):
                        # Note: All operators that set IN_LAMBDA must set
                        # either VALS or RVALS, so we can assume arg is at
                        # least an Lval here
                        blockArgs.append(i)
                    args[i] = arg
            # Modifying lambda functions trumps LIST_EACH and RANGE_EACH
            if blockArgs:
                if not operator.flags & ops.RVALS:
                    args = [self.getRval(arg) if type(arg) is Lval else arg
                            for arg in args]
                if len(blockArgs) == 1:
                    blockArg = blockArgs[0]
                    statements = args[blockArg].getStatements()
                    args[blockArg] = args[blockArg].getReturnExpr()
                    newReturnExpr = [operator] + args
                    return Block(statements, newReturnExpr)
                else:
                    # More than one--they can't have any statements
                    args = [arg.getReturnExpr() if type(arg) is Block else arg
                            for arg in args]
                    newReturnExpr = [operator] + args
                    return Block([], newReturnExpr)
            try:
                if argsToExpand and len(args) == 1:
                    # Single argument to unary op needs expansion
                    result = List(self.evaluate([operator, item])
                                  for item in args[0])
                    #result = List(opFunction(item) for item in args[0])
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
                    result = opFunction(*args)
            except TypeError as e:
                # Probably the wrong number of args
                errMsg = "evaluate(%s) raised TypeError" % expression
                self.err.die("Implementation error:", errMsg, e)
        #!print(fnName, "returned", result)
        return result

    def varTable(self, varName):
        """Returns which table (local or global) a variable resides in."""
        if varName in "abcdefg":
            # Local variable
            return self.locals[self.callDepth]
        else:
            # Global variable
            return self.vars

    def getRval(self, expr):
        #!print("In getRval", repr(expr))
        if type(expr) in (list, tokens.Name):
            expr = self.evaluate(expr)
        if type(expr) in (Scalar, Pattern, List, Range, Block, Nil):
            # Already an rval
            result = expr
        elif type(expr) is ops.Operator:
            # This may happen if we're rval-ing everything in a chained
            # comparison expression
            result = expr
        elif type(expr) is Lval:
            name = expr.name
            if name in self.specialVars:
                # This is a special variable--execute its "get" method
                if "get" in self.specialVars[name]:
                    return self.specialVars[name]["get"]()
                else:
                    self.err.warn("Special var %s does not implement 'get'"
                                  % name)
                    return nil
            else:
                # Get the variable from the appropriate variable table, nil if
                # it doesn't exist
                try:
                    result = self.varTable(name)[name]
                except KeyError:
                    self.err.warn("Referencing uninitialized variable", name)
                    result = nil
                try:
                    for index in expr.sliceList:
                        if type(result) in (List, Scalar):
                            result = result[index]
                        else:
                            self.err.warn("Cannot index into", type(result))
                            return nil
                except IndexError:
                    self.err.warn("Invalid index into %r:" % result, index)
                    return nil
                result = result.copy()
        else:
            self.err.die("Implementation error: unexpected type",
                         type(expr), "in getRval()")
        #!print(result)
        return result

    def assign(self, lval, rval):
        """Sets the value of lval to rval."""
        #!print("In assign,", lval, rval)
        name = lval.name
        if name in self.specialVars:
            # This is a special variable--execute its "get" method
            if lval.sliceList:
                self.err.warn("Cannot assign to slice of special var", name)
                return
            elif "set" not in self.specialVars[name]:
                self.err.warn("Special var %s does not implement 'set'" % name)
                return
            else:
                self.specialVars[name]["set"](rval)
                return

        varTable = self.varTable(name)
        if not lval.sliceList:
            # This is a simple name; just make the assignment
            varTable[name] = rval
            return
        elif name not in varTable:
            # If there is a slicelist, the variable must exist
            self.err.warn("Cannot assign to index of nonexistent variable",
                          name)
            return

        currentVal = varTable[name]
        if type(currentVal) is Range:
            # Can't modify a Range in place... cast it to a List first
            # This way we can do things like r:,9 r@4:42
            currentVal = varTable[name] = List(currentVal)
        
        if type(currentVal) in (List, Scalar):
            # Assignment to a subindex
            #!print("Before assign, variable %r is" % name, currentVal)
            # Dig down through the levels--only works if each level is a List
            # and each index is a single number
            for index in lval.sliceList[:-1]:
                try:
                    currentVal = currentVal[index]
                except IndexError:
                    self.err.warn("Invalid index into %r: %s" % (result, index))
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
            except IndexError:
                self.err.warn("Invalid index into %r: %s" % (currentVal, index))
                return
            #!print("After assign, variable %r is" % name, varTable[name])
        else:
            # Not a subscriptable type
            self.err.warn("Cannot index into", type(varTable[name]))
            return

    def functionCall(self, function, argList):
        """Calls the given function in a new scope with the given arguments."""
        argList = [self.getRval(arg) if type(arg) is Lval else arg
                   for arg in argList]
        # Open a new scope for the function's local variables
        self.callDepth += 1
        self.locals.append({})
        for i, arg in enumerate(argList[:5]):
            variable = Lval("abcde"[i])
            self.assign(variable, arg)
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
        random.seed(rhs)

    ################################
    ### Pip built-in commands    ###
    ################################

    def FOR(self, loopVar, iterable, code):
        """Execute code for each item in iterable, assigned to loopVar."""
        loopVar = Lval(loopVar)
        iterable = self.getRval(iterable)
        if type(iterable) in (List, Range, Scalar):
            for item in iterable:
                self.assign(loopVar, item)
                for statement in code:
                    self.executeStatement(statement)
        else:
            self.err.warn("Cannot iterate over", type(iterable))
            pass
    
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
        elif type(count) is Scalar:
            count = int(count)
        elif type(count) in (List, Range):
            count = len(count)
        for i in range(count):
            for statement in code:
                self.executeStatement(statement)
    
    def OUTPUT(self, expression):
        """Output an expression with NO trailing newline."""
        expression = self.getRval(expression)
        # Because each Pip type implements __str__, we can just print() it
        # However, printing nil has no effect, including on whitespace
        if expression is not nil:
            print(expression, end="")
    
    def PRINT(self, expression):
        """Output an expression with a trailing newline."""
        expression = self.getRval(expression)
        # Because each Pip type implements __str__, we can just print() it
        # However, printing nil has no effect, including on whitespace
        if expression is not nil:
            print(expression)

    def QUERY(self, lval):
        """Get a line from stdin and store it in lval."""
        lval = self.evaluate(lval)
        if type(lval) is not Lval:
            self.err.warn("Attempting to store query input into non-lvalue")
            return
        try:
            line = Scalar(input())
        except EOFError:
            line = nil
        self.assign(lval, line)

    def SWAP(self, lval1, lval2):
        """Exchange the values of two variables (or lvals, in general)."""
        lval1 = self.evaluate(lval1)
        lval2 = self.evaluate(lval2)
        rval1 = self.getRval(lval1)
        rval2 = self.getRval(lval2)
        if type(lval1) is Lval:
            self.assign(lval1, rval2)
        else:
            self.err.warn("Attempting to swap non-lvalue", lval1)
        if type(lval2) is Lval:
            self.assign(lval2, rval1)
        else:
            self.err.warn("Attempting to swap non-lvalue", lval2)

    def TILL(self, cond, code):
        """Loop, executing code, until cond evaluates to true."""
        condVal = self.getRval(cond)
        while not condVal:
            for statement in code:
                self.executeStatement(statement)
            condVal = self.getRval(cond)

    def UNIFY(self, lvals, rval):
        """Unify lvals with items of rval, like Python's tuple unpacking."""
        rval = self.getRval(rval)
        if type(rval) in (List, Scalar, Range):
            for i, lval in enumerate(lvals):
                if i < len(rval):
                    self.assign(self.evaluate(lval), rval[i])
                else:
                    self.assign(self.evaluate(lval), nil)
        else:
            self.err.warn("Unimplemented argtype for UNIFY:", type(rval))
            # TBD: assign nil to all variables, or leave them unmodified?

    def WHILE(self, cond, code):
        """Loop, executing code, while cond evaluates to true."""
        condVal = self.getRval(cond)
        while condVal:
            for statement in code:
                self.executeStatement(statement)
            condVal = self.getRval(cond)

    ###############################
    ### Pip meta-operators      ###
    ###############################

    def FOLD(self, operator, iterable):
        iterable = self.getRval(iterable)
        normalOp = operator.copy()
        normalOp.fold = False
        if type(iterable) in (Scalar, List, Range):
            if len(iterable) == 0:
                return operator.default
            else:
                foldValue = iterable[0]
                for val in iterable[1:]:
                    foldValue = self.evaluate([normalOp, foldValue, val])
                return foldValue
        elif iterable is nil:
            return nil
        else:
            # TODO: allow fold in lambda expressions, e.g. $+_ ?
            self.err.warn("Can't fold", type(iterable))
            return nil
    
    ###############################
    ### Pip built-in operators  ###
    ###############################

    def ABS(self, rhs):
        if type(rhs) is Scalar:
            result = abs(rhs.toNumber())
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtype for ABS:", type(rhs))
            return nil

    def ADD(self, lhs, rhs):
        if type(lhs) is type(rhs) is Scalar:
            result = lhs.toNumber() + rhs.toNumber()
            return Scalar(result)
        elif type(lhs) is Scalar and type(rhs) is Range:
            lower = rhs.getLower() or 0
            upper = rhs.getUpper()
            lower += int(lhs)
            if upper is not None:
                upper += int(lhs)
            return Range(lower, upper)
        elif type(lhs) is Range and type(rhs) is Scalar:
            lower = lhs.getLower() or 0
            upper = lhs.getUpper()
            lower += int(rhs)
            if upper is not None:
                upper += int(rhs)
            return Range(lower, upper)
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
        if type(lhs) is Scalar:
            lhs = List([lhs])
        if type(lhs) in (List, Range):
            result = list(lhs) + [rhs]
            return List(result)
        else:
            self.err.warn("Unimplemented argtypes for APPENDELEM:",
                          type(lhs), "and", type(rhs))
            return nil

    def APPENDLIST(self, lhs, rhs):
        if type(lhs) is Scalar:
            lhs = List([lhs])
        if type(rhs) is Scalar:
            rhs = List([rhs])
        if type(lhs) in (List, Range) and type(rhs) in (List, Range):
            result = list(lhs) + list(rhs)
            return List(result)
        else:
            self.err.warn("Unimplemented argtypes for APPENDLIST:",
                          type(lhs), "and", type(rhs))
            return nil

    def ASC(self, rhs):
        if type(rhs) is Scalar:
            result = ord(str(rhs)[0])
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtype for ASC:", type(rhs))
            return nil

    def ASSIGN(self, lhs, rhs):
        if type(lhs) is not Lval:
            self.err.warn("Attempting to assign to non-lvalue", lhs)
        else:
            # If the rhs is an lval, get its rval
            if type(rhs) is Lval:
                rhs = self.getRval(rhs)
            self.assign(lhs, rhs)
        return lhs

    def AT(self, lhs, rhs):
        if type(rhs) is Lval:
            rhs = self.getRval(rhs)
        
        if type(rhs) is Scalar:
            index = int(rhs)
        elif type(rhs) is Range:
            index = rhs.toSlice()
        elif type(rhs) in (List, Pattern):
            index = rhs
        else:
            self.err.warn("Cannot use", type(rhs), "as index")
            return nil

        if type(lhs) is Lval:
            if type(index) in (int, slice):
                # Indexing using a Scalar or a Range returns an Lval
                return Lval(lhs, index)
            elif type(index) in (List, Pattern):
                # Using a List to index or doing a regex search can only
                # give you an rval
                lhs = self.getRval(lhs)
        
        if type(rhs) is Pattern and type(lhs) is Scalar:
            return List(Scalar(substr)
                        for substr in rhs.getCompiled().findall(str(lhs)))
        elif type(rhs) is Pattern and type(lhs) in (List, Range):
            return List(self.AT(sub, rhs) for sub in lhs)
        elif type(lhs) in (Scalar, List, Range):
            if len(lhs) == 0:
                self.err.warn("Indexing into empty string/list/range")
                return nil
            try:
                return lhs[index]
            except IndexError:
                self.err.warn("Invalid index into %r: %s" % (lhs, index))
                return nil
        else:
            self.err.warn("Cannot index into", type(lhs))
            return nil
    
    def BITWISEAND(self, lhs, rhs):
        if type(lhs) is type(rhs) is Scalar:
            result = int(lhs) & int(rhs)
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtypes for BITWISEAND:",
                          type(lhs), "and", type(rhs))
            return nil

    def BITWISENOT(self, rhs):
        if type(rhs) is Scalar:
            result = ~int(rhs)
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtype for BITWISENOT:", type(rhs))
            return nil

    def BITWISEOR(self, lhs, rhs):
        if type(lhs) is type(rhs) is Scalar:
            result = int(lhs) | int(rhs)
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtypes for BITWISEOR:",
                          type(lhs), "and", type(rhs))
            return nil

    def BITWISEXOR(self, lhs, rhs):
        if type(lhs) is type(rhs) is Scalar:
            result = int(lhs) ^ int(rhs)
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtypes for BITWISEXOR:",
                          type(lhs), "and", type(rhs))
            return nil

    def BLOCK(self, statements):
        if len(statements) > 0 and isExpr(statements[-1]):
            # The last expression is the return value of the function
            returnExpr = statements[-1]
            statements = statements[:-1]
        else:
            returnExpr = None
        return Block(statements, returnExpr)
    
    def CARTESIANPRODUCT(self, list1, list2):
        if list2 is None:
            if type(list1) in (Scalar, List, Range):
                lists = list1
            else:
                self.err.warn("Unimplemented argtype for CARTESIANPRODUCT:",
                              type(list1))
                return nil
        else:
            lists = [list1, list2]
        noniterables = [item for item in lists if type(item) in (Nil, Block)]
        if noniterables:
            # There are some of the "lists" that are not iterable
            # TBD: maybe this can find a non-error meaning?
            self.err.warn("Trying to take CP of non-iterable value(s):",
                          noniterables)
            return nil
        else:
            return List(List(tuple) for tuple in itertools.product(*lists))

    def CAT(self, lhs, rhs):
        if type(lhs) is Scalar and type(rhs) is Scalar:
            result = str(lhs) + str(rhs)
            return Scalar(result)
        elif type(lhs) in (Scalar, Pattern) and type(rhs) in (Scalar, Pattern):
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
            self.err.die("Implementation error: badly formed comparison chain",
                         chain)
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
        if type(rhs) is Scalar:
            result = chr(int(rhs))
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtype for CHR:", type(rhs))
            return nil

    def COORDINATEGRID(self, rows, cols):
        if type(rows) is type(cols) is Scalar:
            rows = range(int(rows))
            cols = range(int(cols))
            return List(List(List([Scalar(row), Scalar(col)])
                             for col in cols)
                        for row in rows)
        else:
            self.err.warn("Unimplemented argtypes for COORDINATEGRID:",
                          type(rows), "and", type(cols))
            return nil
        
    def DEC(self, rhs):
        if type(rhs) is Lval:
            # Subtract one and assign back to rhs
            self.assign(rhs, self.SUB(self.getRval(rhs), scalarOne))
            return rhs
        else:
            self.err.warn("Decrementing non-lvalue", rhs)
            # The expression still evaluates to the value minus one, though
            return self.SUB(rhs, scalarOne)
    
    def DIV(self, lhs, rhs):
        if type(lhs) is Scalar and type(rhs) is Scalar:
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

    def EVAL(self, function, argList=None):
        # TODO: Scalars evaluated as code
        if type(function) is Block and argList is not None:
            return self.functionCall(function, argList)
        elif type(function) is Block and argList is None:
            # Not a function call--just run the block at current scope
            for statement in function.getStatements():
                self.executeStatement(statement)
            return self.evaluate(function.getReturnExpr())
        else:
            self.err.warn("Unimplemented argtypes for EVAL:",
                          type(function), "and", type(argList))
            return nil

    def FILTER(self, function, iterable):
        if type(function) is Block and type(iterable) in (Scalar, List, Range):
            result = (item for item in iterable
                      if self.functionCall(function, [item]))
            return List(result)
        else:
            self.err.warn("Unimplemented argtypes for FILTER:",
                          type(function), "and", type(iterable))
            return nil

    def FIND(self, iterable, item):
        if type(item) is Pattern and type(iterable) is Scalar:
            return Scalar(item.getCompiled().search(str(iterable)).start())
        elif type(iterable) in (Scalar, List, Range):
            return iterable.index(item)
        else:
            self.err.warn("Unimplemented argtypes for FIND:",
                          type(iterable), "and", type(item))
            return nil

    def FINDALL(self, iterable, item):
        if type(item) is List and type(iterable) in (Scalar, Range):
            return List(self.FINDALL(iterable, subitem) for subitem in item)
        elif type(item) is Pattern and type(iterable) is Scalar:
            # Return indices of all regex matches in Scalar
            matches = item.getCompiled().finditer(str(iterable))
            return List(Scalar(match.start()) for match in matches)
        elif (type(item) in (Scalar, Range)
              and type(iterable) in (Scalar, Range, List)
              or type(item) in (List, Pattern) and type(iterable) is List):
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

    def FROMBASE(self, number, base=None):
        if base is None:
            base = 2
        elif type(base) is Scalar:
            base = int(base)
        else:
            self.err.warn("Unimplemented base type for FROMBASE:",
                          type(base))
            return nil
        if base < 2 or base > 36:
            self.err.warn("Invalid base for conversion:", base)
            return nil
        if type(number) is Scalar:
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

    def GROUP(self, iterable, rhs):
        if type(iterable) in (Scalar, List, Range) and type(rhs) is Scalar:
            result = List()
            index = 0
            jump = int(rhs)
            while index < len(iterable):
                result.append(iterable[index:index+jump])
                index += jump
            return result
        else:
            self.err.warn("Unimplemented argtypes for GROUP:",
                          type(iterable), "and", type(rhs))
            return nil

    def IFTE(self, test, trueBranch, falseBranch):
        # Ternary if-then-else operator
        test = self.getRval(test)
        if test:
            return self.evaluate(trueBranch)
        else:
            return self.evaluate(falseBranch)

    def IN(self, lhs, rhs):
        if type(lhs) is Pattern and type(rhs) is Scalar:
            matches = lhs.findall(str(rhs))
            return Scalar(len(matches))
        elif type(rhs) in (Scalar, List, Range):
            return Scalar(rhs.count(lhs))
        else:
            # If it's not one of those types, it's automatically false
            return Scalar("0")

    def INC(self, rhs):
        if type(rhs) is Lval:
            # Add one and assign back to rhs
            self.assign(rhs, self.ADD(self.getRval(rhs), scalarOne))
            return rhs
        else:
            self.err.warn("Incrementing non-lvalue", rhs)
            # The expression still evaluates to the value plus one, though
            return self.ADD(rhs, scalarOne)

    def INTDIV(self, lhs, rhs):
        if type(lhs) is Scalar and type(rhs) is Scalar:
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
    
    def JOIN(self, iterable, sep = None):
        if sep is not None and type(sep) not in (Scalar, Pattern):
            # TBD: does a list as separator give a list of results?
            self.err.warn("Can't join on", type(sep))
            return nil

        if type(iterable) in (Scalar, List, Range):
            result = None
            for item in iterable:
                if type(item) in (List, Range):
                    item = self.JOIN(item, sep)
                if result is None:
                    result = item
                else:
                    if sep is not None:
                        result = self.CAT(result, sep)
                    result = self.CAT(result, item)
            return result
        else:
            if sep is None:
                self.err.warn("Unimplemented argtype for JOIN:",
                              type(iterable))
            else:
                self.err.warn("Unimplemented argtypes for JOIN:",
                              type(iterable), "and", type(sep))
            return nil

    def LEFTOF(self, lhs, rhs):
        # TBD: allow Range or List (of Scalars) as rhs? What would the
        # semantics be?
        if type(rhs) is Lval:
            rhs = self.getRval(rhs)
        if type(lhs) is Lval and type(rhs) is Scalar:
            index = slice(None, int(rhs))
            return Lval(lhs, index)
        elif type(lhs) in (Scalar, List, Range) and type(rhs) is Scalar:
            # Use the lhs's __getitem__ with a slice argument
            return lhs[:int(rhs)]
        else:
            self.err.warn("Unimplemented argtypes for LEFTOF:",
                          type(lhs), "and", type(rhs))
            return nil

    def LEN(self, rhs):
        if type(rhs) in (Scalar, List, Range):
            result = len(rhs)
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtype for LEN:", type(rhs))
            return nil

    def LIST(self, *items):
        return List(items)

    def LOWERCASE(self, rhs):
        if type(rhs) is Scalar:
            return Scalar(str(rhs).lower())
        elif type(rhs) in (Range, Nil):
            return rhs
        else:
            self.err.warn("Unimplemented argtype for LOWERCASE:", type(rhs))
            return nil

    def MAP(self, function, iterable):
        if type(function) is Block and type(iterable) in (Scalar, List, Range):
            result = (self.functionCall(function, [item])
                      for item in iterable)
            return List(result)
        else:
            self.err.warn("Unimplemented argtypes for MAP:",
                          type(function), "and", type(iterable))
            return nil

    def MAPJOIN(self, function, iterable):
        # Same as MAP, but join the result into a string afterwards
        # aMJb == J(aMb)
        return self.JOIN(self.MAP(function, iterable))

    def MAX(self, iterable):
        if type(iterable) in (Scalar, List, Range):
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
        if type(iterable) in (Scalar, List, Range):
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
    
    def MOD(self, lhs, rhs):
        if type(lhs) is Scalar and type(rhs) is Scalar:
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
        if type(lhs) is Scalar and type(rhs) is Scalar:
            result = lhs.toNumber() * rhs.toNumber()
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtypes for MUL:",
                          type(lhs), "and", type(rhs))
            return nil

    def NEG(self, rhs):
        # TODO: Range with negative step value
        if type(rhs) is Scalar:
            result = -rhs.toNumber()
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtype for NEG:", type(rhs))
            return nil

    def NOT(self, rhs):
        result = not rhs
        return Scalar(result)

    def NOTIN(self, lhs, rhs):
        if type(lhs) is Pattern and type(rhs) is Scalar:
            matchExists = lhs.search(str(rhs))
            return Scalar(not matchExists)
        else:
            return Scalar(lhs not in rhs)

    def NUMCMP(self, lhs, rhs):
        # Equivalent to Python2's cmp() function: return -1 if lhs < rhs,
        # 0 if equal, 1 if lhs > rhs
        # Here we can just piggyback off the Pip numeric comparison operators
        if self.NUMGREATER(lhs, rhs):
            return scalarOne
        elif self.NUMLESS(lhs, rhs):
            return Scalar("-1")
        else:
            return Scalar("0")

    def NUMEQUAL(self, lhs, rhs):
        if type(lhs) is type(rhs) is Scalar:
            result = lhs.toNumber() == rhs.toNumber()
        elif type(lhs) is type(rhs) is List:
            result = (len(lhs) == len(rhs)
                      and all(self.NUMEQUAL(i, j)
                              for i, j in zip(lhs, rhs)))
        elif type(lhs) is type(rhs) is Range:
            result = lhs == rhs
        else:
            result = False
        return Scalar(result)

    def NUMGREATER(self, lhs, rhs):
        if type(lhs) is type(rhs) is Scalar:
            result = lhs.toNumber() > rhs.toNumber()
        elif type(lhs) is type(rhs) is List:
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
                result = len(lhs) > len(rhs)
        elif type(lhs) is type(rhs) is Range:
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
        else:
            result = False
        return Scalar(result)

    def NUMGREATEREQ(self, lhs, rhs):
        if type(lhs) is type(rhs) is Scalar:
            result = lhs.toNumber() >= rhs.toNumber()
            return Scalar(result)
        elif type(lhs) is type(rhs) is List:
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
                result = len(lhs) >= len(rhs)
            return Scalar(result)
        elif type(lhs) is type(rhs) is Range:
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
        else:
            return Scalar(False)

    def NUMLESS(self, lhs, rhs):
        if type(lhs) is type(rhs) is Scalar:
            result = lhs.toNumber() < rhs.toNumber()
        elif type(lhs) is type(rhs) is List:
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
                result = len(lhs) < len(rhs)
        elif type(lhs) is type(rhs) is Range:
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
        else:
            result = False
        return Scalar(result)

    def NUMLESSEQ(self, lhs, rhs):
        if type(lhs) is type(rhs) is Scalar:
            result = lhs.toNumber() <= rhs.toNumber()
        elif type(lhs) is type(rhs) is List:
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
                result = len(lhs) <= len(rhs)
        elif type(lhs) is type(rhs) is Range:
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
        else:
            result = False
        return Scalar(result)

    def NUMNOTEQUAL(self, lhs, rhs):
        if type(lhs) is type(rhs) is Scalar:
            result = lhs.toNumber() != rhs.toNumber()
        elif type(lhs) is type(rhs) is List:
            result = (len(lhs) != len(rhs)
                      or any(self.NUMNOTEQUAL(i, j)
                              for i, j in zip(lhs, rhs)))
        elif type(lhs) is type(rhs) is Range:
            result = lhs != rhs
        else:
            result = True
        return Scalar(result)

    def OBJEQUAL(self, lhs, rhs):
        return Scalar(lhs == rhs)

    def OR(self, lhs, rhs):
        # Short-circuiting OR operator
        result = self.getRval(lhs)
        if not result:
            # The lhs was false, so we need to check the rhs
            result = self.getRval(rhs)
        return result
    
    def PARENTHESIZE(self, expr):
        # Result of wrapping a single expression in parentheses
        return expr

    def POS(self, rhs):
        if type(rhs) is Scalar:
            result = rhs.toNumber()
            return Scalar(result)
        elif type(rhs) is Range:
            return rhs
        else:
            self.err.warn("Unimplemented argtype for POS:", type(rhs))
            return nil

    def POW(self, lhs, rhs):
        if type(lhs) is Scalar and type(rhs) is Scalar:
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
                self.err.warn("Can't raise negative number to fractional power")
                return nil
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtypes for POW:",
                          type(lhs), "and", type(rhs))
            return nil

    def PREPENDELEM(self, lhs, rhs):
        # Note that the order of operands has been changed: lhs is now the
        # list, so that one could do lPE:x
        if type(lhs) is Scalar:
            lhs = List([lhs])
        if type(lhs) in (List, Range):
            result = [rhs] + list(lhs)
            return List(result)
        else:
            self.err.warn("Unimplemented argtypes for PREPENDELEM:",
                          type(lhs), "and", type(rhs))
            return nil

    def RANDRANGE(self, lhs, rhs):
        if type(lhs) in (Scalar, Nil) and type(rhs) is Scalar:
            if lhs is nil:
                lhs = 0
            else:
                lhs = lhs.toNumber()
            rhs = rhs.toNumber()
            return Scalar(random.randrange(lhs, rhs))
        else:
            self.err.warn("Unimplemented argtypes for RANDRANGE:",
                          type(lhs), "and", type(rhs))
            return nil
        
    def RANDRANGETO(self, rhs):
        # Unary version of RANDRANGE
        if type(rhs) is Scalar:
            return Scalar(random.randrange(rhs.toNumber()))
        else:
            self.err.warn("Unimplemented argtype for RANDRANGETO:", type(rhs))
            return nil

    def RANGE(self, lhs, rhs):
        if type(lhs) in (Scalar, Nil) and type(rhs) in (Scalar, Nil):
            return Range(lhs, rhs)
        else:
            self.err.warn("Unimplemented argtypes for RANGE:",
                          type(lhs), "and", type(rhs))
            return nil

    def RANGETO(self, rhs):
        # Unary version of RANGE
        if type(rhs) in (Scalar, Nil):
            return Range(nil, rhs)
        else:
            self.err.warn("Unimplemented argtype for RANGETO:", type(rhs))
            return nil

    def REPEATLIST(self, lhs, rhs):
        if type(lhs) in (Scalar, Pattern, Nil):
            lhs = List([lhs])
        if type(lhs) in (List, Range) and type(rhs) is Scalar:
            result = list(lhs.copy()) * int(rhs)
            return List(result)
        else:
            self.err.warn("Unimplemented argtypes for REPEATLIST:",
                          type(lhs), "and", type(rhs))
            return nil

    def REPLACE(self, *args):
        args = (List(arg) if type(arg) is Range else arg for arg in args)
        lhs, old, new = args
        if type(old) is Scalar and type(new) is Pattern:
            old = Pattern(old)
        if (type(lhs) in (List, Scalar)
            and type(old) in (List, Scalar, Pattern)
            and type(new) in (List, Scalar, Pattern, Nil)):
            if type(lhs) is List:
                # Return a List of results
                return List(self.REPLACE(eachLhs, old, new) for eachLhs in lhs)
            elif type(old) is type(new) is List:
                # Both are lists--zip and replace parallel items
                result = lhs
                for eachOld, eachNew in zip(old, new):
                    result = self.REPLACE(result, eachOld, eachNew)
                # Items in the old list that don't correspond to items
                # in the new list should just be deleted
                for eachOld in old[len(new):]:
                    result = self.REPLACE(result, eachOld, nil)
                return result
            elif type(old) is List:
                # Replace each element of old with new
                result = lhs
                for eachOld in old:
                    result = self.REPLACE(result, eachOld, new)
                return result
            elif type(old) is Pattern:
                if type(new) is Pattern:
                    replacement = str(new)
                elif type(new) is Scalar:
                    # If replacing with a literal string, escape all
                    # backslashes first
                    replacement = str(new).replace("\\", "\\\\")
                elif new is nil:
                    replacement = ""
                result = old.getCompiled().sub(replacement, str(lhs))
                return Scalar(result)
            else:
                if new is nil:
                    replacement = ""
                else:
                    # NB: this branch also covers the case type(new) is List
                    # TBD: does that approach make the most sense?
                    replacement = str(new)
                result = str(lhs).replace(str(old), replacement)
                return Scalar(result)
        else:
            self.err.warn("Unimplemented argtypes for REPLACE:",
                          type(lhs), type(old), "and", type(new))
            return nil

    def REMOVE(self, lhs, rhs):
        # TODO: remove List of Scalars from Scalar
        if type(lhs) is Scalar and type(rhs) is Scalar:
            result = str(lhs).translate({ord(c):None for c in str(rhs)})
            return Scalar(result)
        elif type(lhs) in (List, Range):
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
        # Let each class's __repr__ do the work for us
        return Scalar(repr(rhs))

    def REVERSE(self, rhs):
        if type(rhs) is Range:
            rhs = List(rhs)
        if type(rhs) in (Scalar, List):
            # Let those classes' __getitem__ do the work for us
            return rhs[::-1]
        else:
            self.err.warn("Unimplemented argtype for REVERSE:", type(rhs))
            return nil

    def RIGHTOF(self, lhs, rhs):
        # TBD: allow Range or List (of Scalars) as rhs? What would the
        # semantics be?
        if type(rhs) is Lval:
            rhs = self.getRval(rhs)
        if type(lhs) is Lval and type(rhs) is Scalar:
            index = slice(int(rhs), None)
            return Lval(lhs, index)
        elif type(lhs) in (Scalar, List, Range) and type(rhs) is Scalar:
            # Use the lhs's __getitem__ with a slice argument
            return lhs[int(rhs):]
        else:
            self.err.warn("Unimplemented argtypes for RIGHTOF:",
                          type(lhs), "and", type(rhs))
            return nil

    def ROOT(self, lhs, rhs):
        if type(lhs) is Scalar and type(rhs) is Scalar:
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

    def SEND(self, head, *tail):
        # A send-expression's semantics depend on the type of the head:
        # - Block: function call
        # - List, Scalar, Range: subscript
        if type(head) is Lval:
            # Need to check whether it's actually a function
            headRval = self.getRval(head)
            if type(headRval) is Block:
                head = headRval
            # If not, leave it as an lval so the subscripted version can also
            # be an lval
        if type(head) is Block:
            return self.functionCall(head, tail)
        elif type(head) in (Lval, List, Scalar, Range):
            value = head
            for index in tail:
                value = self.AT(value, index)
            return value
        else:
            self.err.warn("Unimplemented argtype for SEND:", type(head))
            return nil

    def SIGN(self, rhs):
        if type(rhs) is Scalar:
            rhs = rhs.toNumber()
            if rhs < 0:
                return Scalar(-1)
            elif rhs > 0:
                return scalarOne
            else:
                return Scalar(0)
        else:
            self.err.warn("Unimplemented argtype for SIGN:", type(rhs))
            return nil

    def SORTNUM(self, iterable):
        if type(iterable) in (Scalar, List, Range):
            try:
                return List(sorted(iterable, key=lambda x:x.toNumber()))
            except TypeError:
                self.err.warn("Cannot sort mixed types in list")
                return nil
        else:
            self.err.warn("Unimplemented argtype for SORTNUM:", type(iterable))
            return nil

    def SORTSTRING(self, iterable):
        if type(iterable) in (Scalar, List, Range):
            # This is going to get a bit wonky when sorting lists of lists,
            # but not sure it's worth the effort to fix
            return List(sorted(iterable, key=str))
        else:
            self.err.warn("Unimplemented argtype for SORTSTRING:",
                          type(iterable))
            return nil

    def SPLIT(self, string, sep=None):
        if type(sep) is Scalar:
            sep = str(sep)
        elif sep is not None:
            # TODO: warning message
            # Some other type, not a valid separator
            return nil
        if type(string) is Scalar:
            if sep is None or sep == "":
                result = (Scalar(char) for char in str(string))
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
        # Splits iterable at given indices
        if type(indices) is Scalar:
            indices = [int(indices)]
        elif type(indices) in (List, Range):
            indices = list(set(int(index) for index in indices))

        if type(iterable) in (List, Scalar, Range) and type(indices) is list:
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
        if type(rhs) is Scalar:
            rhs = rhs.toNumber()
            if rhs < 0:
                # Square root of negative number would be a complex number;
                # for now, return nil
                self.err.warn("Can't take square root of negative number", rhs)
                return nil
            result = rhs ** 0.5
            return Scalar(result)
        else:
            self.err.warn("Unimplemented argtype for SQRT:", type(rhs))
            return nil

    def STR(self, rhs):
        return Scalar(str(rhs))

    def STREQUAL(self, lhs, rhs):
        if type(lhs) in (Scalar, Pattern) and type(rhs) in (Scalar, Pattern):
            result = str(lhs) == str(rhs)
        elif type(lhs) is type(rhs) is List:
            result = (len(lhs) == len(rhs)
                      and all(self.STREQUAL(i, j)
                              for i, j in zip(lhs, rhs)))
        elif type(lhs) is type(rhs) is Range:
            result = lhs == rhs
        else:
            result = False
        return Scalar(result)

    def STRGREATER(self, lhs, rhs):
        if type(lhs) is type(rhs) is Scalar:
            result = str(lhs) > str(rhs)
        elif type(lhs) is type(rhs) is List:
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
        if type(lhs) is type(rhs) is Scalar:
            result = str(lhs) >= str(rhs)
        elif type(lhs) is type(rhs) is List:
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
            result = False
        return Scalar(result)

    def STRLESS(self, lhs, rhs):
        if type(lhs) is type(rhs) is Scalar:
            result = str(lhs) < str(rhs)
        elif type(lhs) is type(rhs) is List:
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
        if type(lhs) is type(rhs) is Scalar:
            result = str(lhs) <= str(rhs)
        elif type(lhs) is type(rhs) is List:
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
            result = False
        return Scalar(result)

    def STRMUL(self, lhs, rhs):
        if type(lhs) in (Scalar, Pattern) and type(rhs) is Scalar:
            string = str(lhs)
            num = rhs.toNumber()
            return type(lhs)(string*num)
        else:
            self.err.warn("Unimplemented argtypes for STRMUL:",
                          type(lhs), "and", type(rhs))
            return nil

    def STRNOTEQUAL(self, lhs, rhs):
        if type(lhs) is type(rhs) is Scalar:
            result = str(lhs) != str(rhs)
        elif type(lhs) is type(rhs) is List:
            result = (len(lhs) != len(rhs)
                      or any(self.STRNOTEQUAL(i, j)
                              for i, j in zip(lhs, rhs)))
        elif type(lhs) is type(rhs) is Range:
            result = lhs != rhs
        else:
            result = True
        return Scalar(result)

    def SUB(self, lhs, rhs):
        if type(lhs) is type(rhs) is Scalar:
            result = lhs.toNumber() - rhs.toNumber()
            return Scalar(result)
        elif type(lhs) is Range and type(rhs) is Scalar:
            lower = lhs.getLower() or 0
            upper = lhs.getUpper()
            lower -= int(rhs)
            if upper is not None:
                upper -= int(rhs)
            return Range(lower, upper)
        else:
            self.err.warn("Unimplemented argtypes for SUB:",
                          type(lhs), "and", type(rhs))
            return nil

    def TOBASE(self, number, base=None):
        # Converts a decimal integer to a string in the specified base
        if base is None:
            base = 2
        elif type(base) is Scalar:
            base = int(base)
        else:
            self.err.warn("Unimplemented base type for TOBASE:",
                          type(base))
            return nil
        if base < 2 or base > 36:
            self.err.warn("Invalid base for conversion:", base)
            return nil
        if type(number) is Scalar:
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
                result = alphabet[number%base] + result
                number //= base
            return Scalar(sign + result)
        else:
            self.err.warn("Unimplemented argtype for TOBASE:",
                          type(number))
            return nil

    def UNIQUE(self, iterable):
        # Removes duplicate values from iterable
        if type(iterable) is List:
            return List(set(iterable))
        elif type(iterable) is Range:
            # All values are already unique
            return iterable
        elif type(iterable) is Scalar:
            return Scalar("".join(set(str(iterable))))
        elif type(iterable) is Nil:
            # This is not a warning--removing duplicates from nil leaves nil
            return nil
        else:
            self.err.warn("Unimplemented argtype for UNIQUE:",
                          type(iterable))
            return nil

    def UPPERCASE(self, rhs):
        if type(rhs) is Scalar:
            return Scalar(str(rhs).upper())
        elif type(rhs) in (Range, Nil):
            return rhs
        else:
            self.err.warn("Unimplemented argtype for UPPERCASE:", type(rhs))
            return nil

    def ZEROGRID(self, rows, cols):
        if type(rows) is type(cols) is Scalar:
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
            if type(list1) in (Scalar, List, Range):
                lists = list1
            else:
                self.err.warn("Trying to zip non-iterable:", type(list1))
                return nil
        else:
            lists = [list1, list2]
        noniterables = [item for item in lists if type(item) in (Nil, Block)]
        if noniterables:
            # There are some of the "lists" that are not iterable
            # TBD: maybe this can find a non-error meaning?
            self.err.warn("Trying to zip non-iterable value(s):",
                          noniterables)
            return nil
        else:
            return List(List(tuple) for tuple in zip(*lists))
    
    def ZIPDEFAULT(self, lists, default=None):
        if default is None:
            default = nil
        if type(lists) in (Scalar, List, Range):
            noniterables = [item for item in lists
                            if type(item) in (Nil, Block)]
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
    def __init__(self, base, sliceValue = None):
        if type(base) is Lval:
            self.name = base.name
            # Make sure to copy the slicelist so changes here don't modify the
            # original
            self.sliceList = base.sliceList[:]
            if sliceValue is not None:
                self.sliceList.append(sliceValue)
        else:
            self.name = str(base)
            self.sliceList = []

    def __str__(self):
        slices = ",".join(map(str, self.sliceList))
        if slices:
            slices = "|" + slices
        return "Lval({})".format(self.name + slices)

    def __eq__(self, rhs):
        if type(rhs) is Lval:
            return self.name == rhs.name and self.sliceList == rhs.sliceList
        elif type(rhs) in (str, tokens.Name):
            return self.name == rhs and self.sliceList == []

