
import sys
import re


class FatalError(Exception):
    """Class for throwing fatal errors."""
    pass

class ErrorReporter:
    """Class for reporting error messages."""
    def __init__(self, warnings=False):
        # The warnings parameter determines whether to print nonfatal
        # errors to stderr or suppress them
        self._warnings = warnings
    
    def warn(self, *message):
        """Print a nonfatal error if warnings are turned on."""
        if self._warnings:
            print(*map(rewritePtypes, message), file=sys.stderr)

    def die(self, *message):
        """Print a fatal error and exit."""
        print(*map(rewritePtypes, message), file=sys.stderr)
        raise FatalError()


def rewritePtypes(message):
    """Convert references to Pip types in a message to just their names."""
    return re.sub(r"<class 'ptypes\.(\w+)'>", r"\1", str(message))
