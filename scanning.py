
import re
import tokens
import operators
from errors import ErrorReporter


err = ErrorReporter(warnings=True)  # TODO: get this setting from the args?

nameRgx = re.compile(r"[A-Z]+|[a-ik-z_]|\$[][()$`'0-9]|\$[a-z][a-z0-9_]*")
stringRgx = re.compile(r'"[^"]*"')
patternRgx = re.compile(r'`([^`\\]|\\.)*`')
charRgx = re.compile(r"'(.|\n)")
escStringRgx = re.compile(r'\\"(\\[^"]|[^\\])*\\"')
numberRgx = re.compile(r'\d+(\.\d+)?')
symbolsRgx = re.compile(r'[][(){};$]|EL')

# Create a regex from a list of operators by re.escape-ing them and joining
# on |
# The list is first sorted from longest to shortest so that "++" matches
# before "+", e.g.
operRgx = re.compile("|".join(
    re.escape(op) for op in sorted(operators.operators, key=len, reverse=True)))

# Combine all regexes together for the grand finale
tokenRgx = re.compile("|".join(rgx.pattern for rgx in [nameRgx,
                                                       stringRgx,
                                                       patternRgx,
                                                       charRgx,
                                                       escStringRgx,
                                                       numberRgx,
                                                       symbolsRgx,
                                                       operRgx]))

#!print(tokenRgx.pattern)

# Single-line comments come in two types: lines that start with a
# (possibly indented) semicolon, and anything after code plus two
# spaces
lineCommentRgx = re.compile(r'(\s*\n\s*;| {2}).*')
# Block comments are anything between {; and ;}
blockCommentRgx = re.compile(r'\{;.*?;\}', re.DOTALL)
whitespaceRgx = re.compile(r'\s+')


def newToken(text, *args, **kwargs):
    "Returns an instance of the correct Token subclass."
    if text in operators.operators:
        return tokens.Operator(text)
    elif text in operators.commands:
        return tokens.Command(text)
    elif text[0] == "'":
        return tokens.Char(text)
    elif text[0] == '"':
        return tokens.String(text)
    elif text[:2] == '\\"':
        return tokens.EscapedString(text)
    elif text[0] == '`':
        return tokens.Pattern(text)
    elif symbolsRgx.fullmatch(text):
        return tokens.Symbol(text)
    elif nameRgx.fullmatch(text):
        return tokens.Name(text)
    elif numberRgx.fullmatch(text):
        return tokens.Number(text)
    # Others as needed
    else:
        return None


def tokenize(code):
    """Returns a list of tokens."""
    tokenList = []
    # Prefix a newline so that leading ;comments get scanned correctly
    code = "\n" + code
    while code:
        if m := lineCommentRgx.match(code):
            # Discard line comments
            #!print("Line comment:", repr(m.group()))
            code = code[m.end():]
        elif m := blockCommentRgx.match(code):
            # Discard block comments
            #!print("Block comment:", repr(m.group()))
            code = code[m.end():]
        elif m := whitespaceRgx.match(code):
            # Discard leading whitespace
            #!print("Whitespace:", repr(m.group()))
            code = code[m.end():]
        elif m := tokenRgx.match(code):
            #!print(m.group())
            text = m.group()
            if text.isalpha() and text.isupper():
                # Break a run of uppercase letters up into two-character
                # chunks, possibly beginning with a single character
                if len(text) % 2 == 1:
                    index = 1
                else:
                    index = 2
                if text == "PIP":
                    index = 3
                text = text[:index]
            else:
                index = m.end()
            if text == "EI":
                # Special-case elseif to make parsing easier: just scan
                # as EL for now, and save the I for the next token
                text = "EL"
                index -= 1
            tokenList.append(newToken(text))
            code = code[index:]
        elif code[0] in '"`\'' or code[:2] == '\\"':
            err.die("Unterminated string or pattern literal:",
                    code.strip())
        else:
            if code[0] == "j":
                err.warn("While scanning, ignored 'j' "
                         "(reserved for future use)")
            else:
                err.warn("While scanning, ignored unrecognized character: "
                         f"{code[0]!r}")
            code = code[1:]
    return tokenList

def scan(code):
    """Tokenize code, and add flags to the end to make parsing easier."""
    tokenList = tokenize(code + "\n")
    # None at end of token list signals to terminate the program
    return tokenList + [None]

def addSpaces(code):
    """Returns a string of code with spaces between tokens.
Useful for debugging."""
    if isinstance(code, str):
        tokens = tokenize(code)
    else:
        # Assume it's already tokenized
        tokens = code
    return " ".join(str(token) for token in tokens if token is not None)

