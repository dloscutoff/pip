
"""Various str wrapper classes for different kinds of tokens."""

class Token:
    "Represents a token from scanning."
    def __init__(self, token):
        self._text = str(token)

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
        return f"{className}({self._text})"

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

class EscapedString(Token):
    pass

class Name(Token):
    pass
    
