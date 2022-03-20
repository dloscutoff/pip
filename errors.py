
import sys
import re


class FatalError(Exception):
    """Class for throwing fatal errors."""
    def __str__(self):
        return " ".join(map(str, self.args))

class BadSyntax(FatalError):
    """Unrecoverable syntax error, e.g. starting with a binary operator."""
    pass

class IncompleteSyntax(FatalError):
    """Recoverable syntax error, e.g. unmatched open parenthesis."""
    pass

class ErrorReporter:
    """Class for reporting error messages."""
    def __init__(self, warnings=False):
        # The warnings parameter determines whether to print nonfatal
        # errors to stderr or suppress them
        self.warnings = warnings
    
    def warn(self, *message):
        """Print a nonfatal error if warnings are turned on."""
        if self.warnings:
            print(*map(rewritePtypes, message), file=sys.stderr)

    def die(self, *message, errorClass=FatalError):
        """Raise a fatal error."""
        raise errorClass(*map(rewritePtypes, message))


def rewritePtypes(message):
    """Convert references to Pip types in a message to just their names."""
    return re.sub(r"<class 'ptypes\.(\w+)'>", r"\1", str(message))
