
import sys

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
        # Covert built-in Pip types in the message to strings of their names
        message = (str(item)[15:-2] if "ptypes" in str(item) else item
                   for item in message)
        if self._warnings:
            print(*message, file=sys.stderr)

    def die(self, *message):
        """Print a fatal error and exit."""
        # Covert built-in Pip types in the message to strings of their names
        message = (str(item)[15:-2] if "ptypes" in str(item) else item
                   for item in message)
        print(*message, file=sys.stderr)
        raise FatalError()
