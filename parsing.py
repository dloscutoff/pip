
import operators
import tokens
import ptypes  # TBD: add another class or two to tokens and refactor this
               # dependency? Would allow ptypes to import isExpr, which might
               # be helpful for Blocks...?
from errors import ErrorReporter


err = ErrorReporter(warnings=True)  # TODO: get this setting from the args?

def parse(tokenList):
    """Parses a list of tokens as a collection of statements."""
    statements = []
    # The last "token" is None, signalling end of program; or, we might meet
    # a closing curly brace at the end of a block
    while tokenList[0] is not None and tokenList[0] != "}":
        statements.append(parseStatement(tokenList))
        #!print(statements)
    return statements
    
def parseStatement(tokenList):
    """Parse a statement from the beginning of the token list."""
    if tokenList[0] is None:
        err.die("Hit end of tokens while parsing statement")
    elif type(tokenList[0]) is tokens.Command:
        token = tokenList.pop(0)
        command = operators.commands[token]
        # Copy Token metadata over
        command.line = token.line
        command.char = token.char
        statement = [command]
        #!print(command, command.argtypes)
        for argtype in command.argtypes:
            if argtype == "ELSE":
                #!print("ELSE", tokenList)
                if tokenList[0] == "E":
                    # Match the E and parse a code block
                    tokenList.pop(0)
                    statement.append(parseBlock(tokenList))
                else:
                    # There is no else branch--use an empty block
                    statement.append([])
            elif argtype == "CODE":
                #!print("CODE", tokenList)
                # Parse a code block
                statement.append(parseBlock(tokenList))
            elif argtype == "NAME":
                # Parse a single name (this is used in FOR loops)
                if type(tokenList[0]) is tokens.Name:
                    statement.append(tokenList.pop(0))
                else:
                    err.die("Expected name, got", tokenList[0])
            else:
                #!print("expression", tokenList)
                # The arg is an expression (whether LVAL or RVAL)
                statement.append(parseExpr(tokenList))
    else:
        # Other than those that start with commands, the only other kinds of
        # statements are bare expressions
        statement = parseExpr(tokenList)
    return statement

def parseBlock(tokenList):
    """Parse either a single statement or a series of statements in {}."""
    if tokenList[0] == "{":
        # Match the curly brace and parse statements until the
        # closing curly brace
        tokenList.pop(0)
        block = parse(tokenList)
        if tokenList[0] == "}":
            tokenList.pop(0)
        elif tokenList[0] is None:
            err.die("Unterminated block")
        else:
            err.die("Expecting } at end of block, got", tokenList[0])
    else:
        # Single statement
        # Have to wrap it in a list to make it a code block
        # that can be passed to execute()
        block = [parseStatement(tokenList)]
    return block

def isExpr(tree):
    """Tests whether the given parse tree is an expression or not."""
    if type(tree) is list and type(tree[0]) is operators.Operator:
        return True
    elif type(tree) in (tokens.Name, ptypes.Scalar):
        return True
    else:
        return False
    
def parseExpr(tokenList, minPrecedence = 0):
    """Parse an expression from the beginning of the token list."""
    expression = parseOperand(tokenList)
    while type(tokenList[0]) is tokens.Operator:
        op = tokenList[0]
        if op in operators.opsByArity[2]:
            op = operators.opsByArity[2][op]
        elif op in operators.opsByArity[3]:
            op = operators.opsByArity[3][op]
        else:
            # Probably a unary operator beginning another expression
            break
        if op.precedence < minPrecedence:
            # Done parsing a higher-precedence subexpression--we'll get
            # this operator at an outer level of recursion
            break
        # Once it's checked out, take it off the tokens list
        tokenList.pop(0)
        # Check whether the next token is the : meta-operator
        if tokenList[0] == ":":
            # If so, turn this into a compute-and-assign operation
            tokenList.pop(0)
            op = op.copy()
            op.assign = True
            assignOp = operators.opsByArity[2][":"]
            op.precedence = assignOp.precedence
            op.associativity = assignOp.associativity
        # Add operator as root of expression tree
        if op.associativity == "C":
            # Special-case chaining comparison operators
            expression = [operators.chain, expression]
        else:
            expression = [op, expression]
        #!print(">>> Before bubble:", expression)
        # Rearrange the tree according to operator's precedence/associativity
        expression, activeTree = bubble(expression)
        #!print(">>> After bubble:", expression)
        # activeTree is the (sub)tree whose root is the operator we just
        # parsed; add the next operand as its rightmost child
        if op.associativity == "C":
            # Also add the comparison operator itself for a comparison chain
            activeTree.append(op)
        elif op.arity == 3:
            # Parse the middle expression of a ternary operator
            activeTree.append(parseExpr(tokenList))
        activeTree.append(parseOperand(tokenList))

    # ; is an optional statement terminator
    if minPrecedence == 0 and tokenList[0] == ";":
        tokenList.pop(0)
    return expression


def bubble(exprTree):
    """Moves the root operator down to its proper position, given precedence."""
    # There is no right operand yet
    op = exprTree[0]
    left = exprTree[1]
    if type(left) is list:
        nextOp = left[0]
        if (nextOp.arity in (2, 3)
            and (nextOp.precedence < op.precedence
                 or (nextOp.precedence == op.precedence
                     and op.associativity == 'R'))):
            # In essence, this swaps the current root out for the operator
            # which is the root of its left child. The details are hard to
            # follow without a diagram, though. See below.
            leftRight = left[-1]
            exprTree = left
            newTree = [op, leftRight]
            newTree, activeTree = bubble(newTree)
            exprTree[-1] = newTree
            # We return the reassembled tree, as well as the subtree rooted at
            # the current operator (where the next operand will be added)
            return exprTree, activeTree
        elif op is operators.chain and nextOp is operators.chain:
            # Combine two chaining operators into one
            return left, left
    return exprTree, exprTree

#    *       +
#   /       / \
#  +    ->  1  *
# / \         /
# 1 2         2


def parseOperand(tokenList):
    """Parse a Scalar, a unary expression, or a grouped expression."""
    if type(tokenList[0]) is tokens.Name:
        # For a Name token, just return it
        return tokenList.pop(0)
    elif type(tokenList[0]) is tokens.String:
        # Strip the double-quotes off a literal string (TODO: backslash escapes)
        return ptypes.Scalar(tokenList.pop(0)[1:-1])
    elif type(tokenList[0]) is tokens.Char:
        # Single-quoted character
        return ptypes.Scalar(tokenList.pop(0)[1])
    elif type(tokenList[0]) is tokens.Number:
        return ptypes.Scalar(tokenList.pop(0))
    elif tokenList[0] == "(":
        # Parse a parenthesized expression: nil, group, or send-expression
        tokenList.pop(0)
        expressions = []
        while tokenList[0] != ")":
            if tokenList[0] is None:
                err.die("Unterminated parenthesis")
            else:
                expressions.append(parseExpr(tokenList))
        # Remove the closing parenthesis
        tokenList.pop(0)
        if len(expressions) == 0:
            # () is equivalent to nil
            return ptypes.nil
        elif len(expressions) == 1:
            # Exactly one expression in parentheses: a group
            return [operators.group, expressions[0]]
        else:
            # Multiple expressions: a send-expression (function call or
            # iterable subscript) such as (f 1 2 3)
            return [operators.send] + expressions
    elif tokenList[0] == "[":
        # Parse a list constructor
        tokenList.pop(0)
        subExpression = [operators.enlist]
        while tokenList[0] != "]":
            if tokenList[0] is None:
                err.die("Unterminated list")
            else:
                subExpression.append(parseExpr(tokenList))
        tokenList.pop(0)
        return subExpression
    elif tokenList[0] == "{":
        # Parse a code block
        statements = parseBlock(tokenList)
        return [operators.block, statements]
    elif tokenList[0] in operators.opsByArity[1]:
        # Parse a unary operator followed by its operand
        token = tokenList.pop(0)
        op = operators.opsByArity[1][token]
        # Check whether the next token is the : meta-operator
        if tokenList[0] == ":":
            # If so, turn this into a compute-and-assign operation
            tokenList.pop(0)
            op = op.copy()
            op.assign = True
        # TODO: copy Token metadata over for error-reporting purposes
        #op.line = token.line
        #op.char = token.char
        subOperand = parseExpr(tokenList, minPrecedence = op.precedence)
        return [op, subOperand]
    elif tokenList[0] == "$":
        # The fold meta-operator is modifying a subsequent binary operator
        tokenList.pop(0)
        if tokenList[0] in operators.opsByArity[2]:
            token = tokenList.pop(0)
            op = operators.opsByArity[2][token]
            op = op.copy()
            op.fold = True
            subOperand = parseExpr(tokenList, minPrecedence = op.precedence)
            return [op, subOperand]
        else:
            err.die("Missing operator for $ meta-operator? Got",
                    tokenList[0], "instead")
            
    # TODO: meta-operator \ is also unary
    
    # If control reaches here, we've got a problem
    if tokenList[0] is None:
        err.die("Hit end of tokens while parsing expression")
    elif (tokenList[0] in operators.opsByArity[2] or
          tokenList[0] in operators.opsByArity[3]):
        err.die(tokenList[0], "is not a unary operator")
    else:
        err.die("Expected expression, got", tokenList[0])



