
import re
import operators
import tokens
import ptypes  # TBD: add another class or two to tokens and refactor this
               # dependency? Would allow ptypes to import isExpr, which might
               # be helpful for Blocks...?
from errors import ErrorReporter

assignOp = operators.opsByArity[2][":"]
err = ErrorReporter(warnings=True)  # TODO: get this setting from the args?

def parse(tokenList):
    "Parses a list of tokens as a collection of statements."
    statements = []
    # The last "token" is None, signalling end of program; or, we might meet
    # a closing curly brace at the end of a block
    while tokenList[0] is not None and tokenList[0] != "}":
        statements.append(parseStatement(tokenList))
        #!print(statements)
    return statements
    
def parseStatement(tokenList):
    "Parse a statement from the beginning of the token list."
    if tokenList[0] is None:
        err.die("Hit end of tokens while parsing statement")
    elif isinstance(tokenList[0], tokens.Command):
        token = tokenList.pop(0)
        command = operators.commands[token]
        statement = [command]
        #!print(command, command.argtypes)
        for argtype in command.argtypes:
            if argtype == "ELSE":
                #!print("ELSE", tokenList)
                if tokenList[0] == "EL":
                    # Match the EL and parse a code block
                    tokenList.pop(0)
                    statement.append(parseBlock(tokenList))
                else:
                    # There is no else branch--use an empty block
                    statement.append([])
            elif argtype == "CODE":
                #!print("CODE", tokenList)
                # Parse a code block
                statement.append(parseBlock(tokenList))
            elif argtype == "LOOPVAR":
                # Parse a single name or a (possibly nested) list of
                # names (used in FOR loops)
                statement.append(parseNameList(tokenList))
            else:
                #!print("expression", tokenList)
                # The arg is an expression (whether LVAL or RVAL)
                statement.append(parseExpr(tokenList))
    else:
        # Other than those that start with commands, the only other kinds of
        # statements are bare expressions
        statement = parseExpr(tokenList)
    # A semicolon after a statement is unnecessary but legal
    if tokenList[0] == ";":
        tokenList.pop(0)
    return statement

def parseNameList(tokenList):
    "Parse a (possibly nested) list containing names."
    if isinstance(tokenList[0], tokens.Name):
        nameList = tokenList.pop(0)
    elif tokenList[0] == "[":
        tokenList.pop(0)
        nameList = [operators.enlist]
        while tokenList[0] != "]":
            nameList.append(parseNameList(tokenList))
        tokenList.pop(0)
        if len(nameList) == 1:
            # No names in the list, just the enlist operator
            err.die("List of names in for-loop header cannot be empty")
    elif tokenList[0] is None:
        err.die("Unterminated list of names in for-loop header")
    else:
        err.die("For-loop header must be name or list of names, not",
                tokenList[0])
    # A semicolon after the name list is unnecessary but legal
    if tokenList[0] == ";":
        tokenList.pop(0)
    return nameList

def parseBlock(tokenList):
    "Parse either a single statement or a series of statements in {}."
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
    "Tests whether the given parse tree is an expression or not."
    if isinstance(tree, list) and isinstance(tree[0], operators.Operator):
        return True
    elif isinstance(tree, (tokens.Name, ptypes.Scalar,
                           ptypes.Pattern, ptypes.Nil)):
        return True
    else:
        return False
    
def parseExpr(tokenList, minPrecedence=-1):
    "Parse an expression from the beginning of the token list."
    expression = parseOperand(tokenList)
    while isinstance(tokenList[0], tokens.Operator):
        op = tokenList[0]
        if op in operators.opsByArity[2]:
            op = operators.opsByArity[2][op]
        elif op in operators.opsByArity[3]:
            op = operators.opsByArity[3][op]
        else:
            # Probably a unary operator beginning another expression
            break
        precedence = op.precedence
        if tokenList[1] == ":":
            # The next token is the : meta-operator, which lowers the
            # precedence
            precedence = assignOp.precedence
        if precedence < minPrecedence:
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
    if minPrecedence == -1 and tokenList[0] == ";":
        tokenList.pop(0)
    return expression


def bubble(exprTree):
    "Moves the root operator down to its proper position, given precedence."
    # There is no right operand yet
    op = exprTree[0]
    left = exprTree[1]
    if isinstance(left, list):
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
    "Parse a name, literal, unary expression, or parenthesized expression."
    if isinstance(tokenList[0], tokens.Name):
        # For a Name token, just return it
        return tokenList.pop(0)
    elif isinstance(tokenList[0], tokens.String):
        # Strip the double-quotes off a literal string
        return ptypes.Scalar(tokenList.pop(0)[1:-1])
    elif isinstance(tokenList[0], tokens.Pattern):
        # Strip off backticks and simplify \` inside
        # `\1\\\`2\\` -> \1\\`2\\
        rawPattern = tokenList.pop(0)[1:-1].replace("\\`", "`")
        return ptypes.Pattern(rawPattern)
    elif isinstance(tokenList[0], tokens.Char):
        # Single-quoted character
        return ptypes.Scalar(tokenList.pop(0)[1])
    elif isinstance(tokenList[0], tokens.EscapedString):
        # \"String\" that allows for double quotes and limited interpolation
        # Strip off \" delimiters
        rawText = tokenList.pop(0)[2:-2]
        # Parse any interpolation sequences (for now, just names)
        litOrInterpolation = re.split(r"\\([a-z_]|[A-Z]{1,2})", rawText)
        if len(litOrInterpolation) == 1:
            # No interpolations--just return a Scalar
            return ptypes.Scalar(rawText.replace(r"\\", "\\"))
        else:
            # Translate the interpolations into a parse tree
            strOp = operators.opsByArity[1]["ST"]
            joinOp = operators.opsByArity[1]["J"]
            expression = [operators.enlist]
            literal = litOrInterpolation.pop(0)
            expression.append(ptypes.Scalar(literal.replace(r"\\", "\\")))
            while litOrInterpolation:
                interpolation = litOrInterpolation.pop(0)
                expression.append([strOp, tokens.Name(interpolation)])
                literal = litOrInterpolation.pop(0)
                expression.append(ptypes.Scalar(literal.replace(r"\\", "\\")))
            return [operators.paren, [joinOp, expression]]
    elif isinstance(tokenList[0], tokens.Number):
        return ptypes.Scalar(tokenList.pop(0))
    elif tokenList[0] == "(":
        # Parse a parenthesized expression: nil, grouped expr, or send-expr
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
            # Exactly one expression in parentheses
            return [operators.paren, expressions[0]]
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
    elif (tokenList[0] in operators.opsByArity[1]
          or tokenList[0] == "$"):
        # Parse a unary operator followed by its operand
        token = tokenList.pop(0)
        if token == "$":
            # The fold meta-operator is modifying a subsequent binary operator
            if tokenList[0] in operators.opsByArity[2]:
                token = tokenList.pop(0)
                op = operators.opsByArity[2][token]
                op = op.copy()
                op.fold = True
                op.arity = 1
            else:
                err.die("Missing/wrong operator for $ meta-operator: got",
                        tokenList[0], "instead")
        else:
            op = operators.opsByArity[1][token]
        # Check for the * and : meta-operators
        if tokenList[0] == "*":
            # Turn this into a map-each operation
            tokenList.pop(0)
            op = op.copy()
            op.map = True
        if tokenList[0] == ":":
            # Turn this into a compute-and-assign operation
            tokenList.pop(0)
            op = op.copy()
            op.assign = True
            op.precedence = assignOp.precedence
        subOperand = parseExpr(tokenList, minPrecedence=op.precedence+1)
        return [op, subOperand]
            
    # If control reaches here, we've got a problem
    if tokenList[0] is None:
        err.die("Hit end of tokens while parsing expression")
    elif (tokenList[0] in operators.opsByArity[2] or
          tokenList[0] in operators.opsByArity[3]):
        err.die(tokenList[0], "is not a unary operator")
    else:
        err.die("Expected expression, got", repr(tokenList[0]))


def unparse(tree, statementSep=""):
    "Convert parse tree back to string of code."
    code = ""
    for statement in tree:
        if isinstance(statement, list):
            # A parse tree
            if isinstance(statement[0], operators.Operator):
                code += unparseExpr(statement, statementSep)
            elif isinstance(statement[0], operators.Command):
                code += str(statement[0])
                for i, argtype in enumerate(statement[0].argtypes):
                    arg = statement[1+i]
                    if argtype == "ELSE":
                        code += " EL"
                        argtype = "CODE"
                    if argtype == "LOOPVAR" or argtype == "EXPR":
                        code += " " + unparseExpr(arg, statementSep)
                    elif argtype == "CODE":
                        code += " {" + unparse(arg, statementSep) + "}"
        else:
            # Not a parse tree, probably a literal or name
            code += unparseExpr(statement, statementSep)
        code += "; "
        code += statementSep
    return code.strip("; " + statementSep)

def unparseExpr(tree, statementSep):
    "Convert parse tree of expression back to string of code."
    #!print("Unparsing expr", tree)
    if isinstance(tree, list):
        op = tree[0]
        if op == "PAREN":
            code = "(" + unparseExpr(tree[1], statementSep) + ")"
        elif op == "BLOCK":
            code = "{" + unparse(tree[1], statementSep) + "}"
        elif op == "SEND":
            exprs = (unparseExpr(item, statementSep) for item in tree[1:])
            code = "(" + "; ".join(exprs) + ")"
        elif op == "LIST":
            exprs = (unparseExpr(item, statementSep) for item in tree[1:])
            code = "[" + "; ".join(exprs) + "]"
        else:
            operands = []
            for item in tree[1:]:
                if isinstance(item, operators.Operator):
                    operands.append(str(item))
                else:
                    operand = unparseExpr(item, statementSep)
                    if (isinstance(item, list)
                            and item[0] not in ["PAREN", "BLOCK",
                                                "SEND", "LIST"]):
                        operand = "(" + operand + ")"
                    operands.append(operand)
            if op == "CHAIN":
                code = " ".join(operands)
            elif op.arity == 1:
                code = f"{op} {operands[0]}"
            elif op.arity == 2:
                code = f"{operands[0]} {op} {operands[1]}"
            elif op.arity == 3:
                code = f"{operands[0]} {op} {operands[1]}; {operands[2]}"
    elif isinstance(tree, tokens.Name):
        code = str(tree)
    elif isinstance(tree, (ptypes.Scalar, ptypes.Pattern, ptypes.Nil)):
        code = repr(tree)
    else:
        # Shouldn't ever get here
        raise ValueError(repr(tree))
    return code

