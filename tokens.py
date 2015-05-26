
"""Various str wrapper classes for different kinds of tokens."""

class Token:
    """Stores a token with line & char number metadata."""
    def __init__(self, token, line = None, char = None):
        self._text = str(token)
        if type(token) is Token:
            # Copy constructor--take metadata from the previous token unless
            # overridden here
            line = line if line is not None else token.line
            char = char if char is not None else token.char
        self.line = line
        self.char = char

    def __str__(self):
        return self._text

    def __eq__(self, rhs):
        return rhs == self._text

    def __getitem__(self, rhs):
        return self._text[rhs]

    def __hash__(self):
        return hash(self._text)
    
    def __repr__(self):
        # Grab just the name of the (sub)class from type(self)
        className = str(type(self))[8:-2]
        if className[:7] == "tokens.":
            className = className[7:]
        return "{}({},{},{})".format(className,
                                       self._text,
                                       self.line,
                                       self.char)

class Command(Token):
    pass

class Operator(Token):
    pass

class Symbol(Token):
    pass

class Number(Token):
    pass

class String(Token):
    pass

class Pattern(Token):
    pass

class Char(Token):
    pass

class Name(Token):
    pass
    
