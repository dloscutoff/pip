
import re
import tokens
import operators
from errors import ErrorReporter


err = ErrorReporter(warnings=True)  # TODO: get this setting from the args?

nameRgx = re.compile(r'[A-Z]+|[a-z_]')
stringRgx = re.compile(r'"[^"]*"')
charRgx = re.compile(r"'.")
numberRgx = re.compile(r'[0-9]+(\.[0-9]+)?')
symbolsRgx = re.compile(r'[][(){};$]')

# Create a regex from a list of operators by re.escape-ing them and joining
# on |
# The list is first sorted from longest to shortest so that "++" matches
# before "+", e.g.
operRgx = re.compile("|".join(
    re.escape(op) for op in sorted(operators.operators, key=len, reverse=True)))

# Combine all regexes together for the grand finale
tokenRgx = re.compile("|".join(rgx.pattern for rgx in [nameRgx,
                                                       stringRgx,
                                                       charRgx,
                                                       numberRgx,
                                                       symbolsRgx,
                                                       operRgx]))

#!print(tokenRgx.pattern)

# Comments come in two types: lines that start with a (possibly indented)
# semicolon, and anything after code plus two or more spaces
commentRgx = re.compile(r'\s*\n\s*;.*?$| {2,}.*?$', re.MULTILINE)
whitespaceRgx = re.compile(r'\s+')


def newToken(text, *args, **kwargs):
    """Returns an instance of the correct Token subclass."""
    # Maybe there's a better way to do this?
    if text in operators.operators:
        return tokens.Operator(text, *args, **kwargs)
    elif text in operators.commands:
        return tokens.Command(text, *args, **kwargs)
    elif nameRgx.match(text):
        return tokens.Name(text, *args, **kwargs)
    elif stringRgx.match(text):
        return tokens.String(text, *args, **kwargs)
    elif charRgx.match(text):
        return tokens.Char(text, *args, **kwargs)
    elif numberRgx.match(text):
        return tokens.Number(text, *args, **kwargs)
    elif symbolsRgx.match(text):
        return tokens.Symbol(text, *args, **kwargs)
    # Others as needed
    else:
        return None


def tokenize(code):
    """Returns a list of tokens."""
    tokenList = []
    # Prefix a newline so that leading ;comments get scanned correctly
    code = "\n" + code
    while code:
        m = commentRgx.match(code)
        if m:
            # Discard comments
            print(repr(m.group()))
            code = code[m.end():]
        elif whitespaceRgx.match(code):
            # Discard leading whitespace
            code = code.lstrip()
        else:
            m = tokenRgx.match(code)
            if m:
                #!print(m.group())
                text = m.group()
                index = m.end()
                if text == "EI":
                    # Special-case elseif to make parsing easier: just scan
                    # the E for now, and save the I for the next token
                    text = "E"
                    index -= 1
                # TODO: Line and character numbers
                tokenList.append(newToken(text))
                code = code[index:]
            elif code[0] in '"':
                err.die("Unterminated string literal: %s" % code.strip())
            else:
                err.warn("While scanning, ignored unrecognized character: %r"
                         % code[0])
                code = code[1:]
    return tokenList

def scan(code):
    """Tokenize code, and add flags to the end to make parsing easier."""
    tokenList = tokenize(code)
    # None at end of token list signals to terminate the program
    return tokenList + [None]

def addSpaces(code):
    """Returns a string of code with spaces between tokens.
Useful for debugging."""
    if type(code) is str:
        tokens = tokenize(code)
    else:
        # Assume it's already tokenized
        tokens = code
    return " ".join(str(token) for token in tokens if token is not None)

