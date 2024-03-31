
"""Classes for Pip data types."""

import re
import itertools
import sys
import types

import tokens
import parsing

zeroRgx = re.compile(r"^(0+(\.0*)?|0*\.0+)$")
exp = r"(?:e[+-]\d+)"
pyFloatRgx = re.compile(rf"-?(\d+\.\d*{exp}?|\d+{exp}|\.\d+{exp}?)")
pipFloatRgx = re.compile(r"-?\d+\.\d+")
intRgx = re.compile(r"-?\d+")


class PipType:
    """Base class for all Pip types."""
    
    def __hash__(self):
        return hash(repr(self))


class PipIterable(PipType):
    """Base class for all iterable Pip types."""
    pass


class Scalar(PipIterable):
    """Represents a string or number."""
    
    def __init__(self, value=""):
        # Store the value as a Python string
        if isinstance(value, bool):
            # Convert to an integer first
            value = int(value)
        elif isinstance(value, float) and str(value).endswith(".0"):
            # Convert float with no fractional part to integer
            value = int(value)
        self._value = str(value)

    def copy(self):
        return Scalar(self._value)

    def isEmpty(self):
        return self._value == ""

    def isFinite(self):
        return True

    def __str__(self):
        return self._value

    def __repr__(self):
        m = pipFloatRgx.match(self._value) or intRgx.match(self._value)
        if m and m.end() == len(self._value):
            # Scalars that match the format of Pip numeric literals
            # can be displayed without quotes
            return self._value
        else:
            # Other Scalars, even those that can evaluate to a number,
            # must have quotes
            if '"' in self._value:
                # Use escaped-string format
                return r'\"' + self._value.replace("\\", r"\\") + r'\"'
            else:
                # Use normal string format
                return f'"{self._value}"'

    def __int__(self):
        if m := intRgx.match(self._value.lstrip()):
            return int(m.group())
        else:
            return 0

    def __bool__(self):
        """A Scalar is false iff it is empty string or some form of 0."""
        return not self.isEmpty() and not zeroRgx.match(self._value)

    def __eq__(self, rhs):
        return type(self) is type(rhs) and self._value == rhs._value

    __hash__ = PipIterable.__hash__

    def __len__(self):
        return len(self._value)

    def toNumber(self):
        """Convert to a Python float or int for math purposes."""
        if m := pyFloatRgx.match(self._value.lstrip()):
            return float(m.group())
        elif m := intRgx.match(self._value.lstrip()):
            return int(m.group())
        else:
            # If it doesn't match a float or an int, its numeric value is 0
            return 0

    def __contains__(self, item):
        if isinstance(item, (str, Scalar)):
            return str(item) in self._value
        else:
            return False

    def __getitem__(self, index):
        if isinstance(index, List):
            return List(self[i] for i in index)
        elif isinstance(index, Scalar):
            index = int(index)
        elif isinstance(index, Range):
            index = index.toSlice()
        
        if isinstance(index, int):
            if self._value == "":
                raise IndexError("Cannot index into empty string.")
            else:
                index %= len(self._value)
                return Scalar(self._value[index])
        elif isinstance(index, slice):
            if self._value == "":
                # Slicing the empty string gives empty string
                return self
            length = len(self._value)
            repString = self._value
            start = index.start if index.start is not None else 0
            stop = index.stop if index.stop is not None else length
            if start < 0:
                start += length
            if stop < 0:
                stop += length
            if start >= stop:
                return Scalar("")
            while start < 0:
                repString = self._value + repString
                start += length
                stop += length
            while stop > len(repString):
                repString += self._value
            return Scalar(repString[start:stop])
        else:
            raise TypeError(f"Cannot use {type(index)} to index Scalar")

    def __setitem__(self, index, item):
        # Behold! Mutable strings!
        if isinstance(index, int):
            index %= len(self._value)
        value = list(self._value)
        value.__setitem__(index, str(item))
        self._value = ''.join(value)

    def __iter__(self):
        for char in self._value:
            yield Scalar(char)

    def count(self, substring):
        if isinstance(substring, Scalar):
            return self._value.count(substring._value)
        else:
            return nil

    def index(self, searchItem, startIndex=0):
        if not isinstance(searchItem, Scalar):
            raise TypeError("Scalar.index requires Scalar argument, "
                            f"not {type(searchItem)}")
        else:
            try:
                return self._value.index(searchItem._value, startIndex)
            except ValueError:
                raise ValueError(f"{searchItem} is not in Scalar at index "
                                 f">= {startIndex}")


class Pattern(PipType):
    """Represents a regular expression or substitution pattern."""

    def __init__(self, value="", flags=0):
        if isinstance(value, Pattern):
            self._raw = value._raw
            # Copy flags from the other pattern, then toggle any flags
            # specified by the argument
            self._flags = value._flags ^ flags
        else:
            self._raw = str(value)
            self._flags = flags
        self._compiled = None
        self._separator = None

    def asRegex(self):
        if not self._compiled:
            # Add an extra <all> capture group around the whole thing
            pyRegex = f"(?P<all>{self._raw})"
            # Increment the numbers of all back-references
            pyRegex = re.sub(r"(?<!\\)((?:\\{2})*)\\([1-9]\d?)",
                             lambda m: (m.group(1)
                                        + "\\"
                                        + str(int(m.group(2))+1)),
                             pyRegex)
            self._compiled = re.compile(pyRegex, self._flags)
        return self._compiled

    def asSeparator(self):
        if not self._separator:
            self._separator = re.compile(self._raw, self._flags)
        return self._separator

    def asReplacement(self):
        # Increment all back-references
        pyReplace = re.sub(r"(?<!\\)((?:\\{2})*)\\([1-9]\d?)",
                           lambda m: (m.group(1)
                                      + "\\"
                                      + str(int(m.group(2))+1)),
                           self._raw)
        # Turn unescaped ampersands into back-references to the whole match
        pyReplace = re.sub(r"(?<!\\)((?:\\{2})*)&",
                           lambda m: m.group(1) + r"\g<all>",
                           pyReplace)
        # Turn escaped ampersands into literal ampersands
        pyReplace = pyReplace.replace("\\&", "&")
        return pyReplace

    def copy(self):
        copy = Pattern(self)
        copy._compiled = self._compiled
        copy._separator = self._separator
        return copy

    def wrap(self):
        if len(self._raw) > 1:
            return Pattern(f"(?:{self._raw})", self._flags)
        else:
            # A length-1 regex doesn't need to be wrapped
            return self

    def group(self):
        return Pattern(f"({self._raw})", self._flags)

    def kleeneStar(self):
        return Pattern(f"{self._raw}*", self._flags)

    def plus(self):
        return Pattern(f"{self._raw}+", self._flags)

    def repeat(self, count):
        if isinstance(count, int):
            return Pattern(f"{self._raw}{{{count}}}", self._flags)
        else:
            raise TypeError(f"Cannot repeat Pattern by {type(count)}")

    def concat(self, other):
        return Pattern(f"{self._raw}{other._raw}",
                       self._flags | other._flags)

    def alternate(self, other):
        return Pattern(f"{self._raw}|{other._raw}",
                       self._flags | other._flags)

    def isEmpty(self):
        return self._raw == ""

    def isFinite(self):
        return True

    def __str__(self):
        return self._raw

    def __repr__(self):
        flags = ""
        if self._flags & re.ASCII:
            flags += "A"
        if self._flags & re.IGNORECASE:
            flags += "-"
        if self._flags & re.MULTILINE:
            flags += ","
        if self._flags & re.DOTALL:
            flags += "."
        return flags + "`" + self._raw.replace("`", r"\`") + "`"

    def __bool__(self):
        """A Pattern is false iff it is empty."""
        return not self.isEmpty()

    def __eq__(self, rhs):
        return (type(rhs) == type(self)
                and self._raw == rhs._raw
                and self._flags == rhs._flags)

    __hash__ = PipType.__hash__

    def __len__(self):
        return len(self._raw)

    def toNumber(self):
        return 0
    
    def __iter__(self):
        for char in self._raw:
            yield Scalar(char)


class List(PipIterable):
    """Represents a list of objects."""

    # How to format a list when outputting it
    # Options are the same as the associated command-line flags:
    # None: Join on empty string
    # n: Join on newline
    # p: Pretty-print: use the repr instead (useful for debugging)
    # s: Join on space
    # l: Print as multiple lines, with each line joined on empty string
    # P: Print as multiple lines, with each line repr'd
    # S: Print as multiple lines, with each line joined on space
    outFormat = None

    def __init__(self, value=None):
        # TODO: make this more robust to handle infinite Range or range
        if isinstance(value, (Range,
                              tuple,
                              set,
                              types.GeneratorType,
                              map,
                              zip,
                              itertools.starmap,
                              )):
            self._value = [item for item in value]
        elif isinstance(value, (List, list)):
            self._value = [item.copy() for item in value]
        elif isinstance(value, range):
            self._value = [Scalar(item) for item in value]
        elif value is None:
            self._value = []
        else:
            self._value = []
            raise TypeError(f"Cannot convert {type(value)} to List")

    def copy(self):
        return List(item.copy() for item in self._value)

    def isEmpty(self):
        return self._value == []

    def isFinite(self):
        return True

    def __str__(self):
        # How a List is formatted depends on the command-line flags
        if not self.outFormat:
            # Default: concatenate all items together
            return "".join(str(i) for i in self._value)
        elif self.outFormat == "p":
            return repr(self)
        elif self.outFormat == "n":
            return "\n".join(str(i) for i in self._value)
        elif self.outFormat == "s":
            return " ".join(str(i) for i in self._value)
        elif self.outFormat == "l":
            # Each item in the list is a line, which in turn is joined on
            # empty string
            return "\n".join(i.joined("") if isinstance(i, List) else str(i)
                             for i in self._value)
        elif self.outFormat == "P":
            # Each item in the list is a line, which in turn is repr'd
            return "\n".join(repr(i) for i in self._value)
        elif self.outFormat == "S":
            # Each item in the list is a line, which in turn is joined on
            # space
            return "\n".join(i.joined(" ") if isinstance(i, List) else str(i)
                             for i in self._value)

    def joined(self, separator):
        return separator.join(i.joined(separator)
                              if isinstance(i, List)
                              else str(i)
                              for i in self._value)

    def __repr__(self):
        return "[" + ";".join(repr(i) for i in self._value) + "]"

    def __bool__(self):
        return not self.isEmpty()

    def __eq__(self, rhs):
        return type(rhs) == type(self) and self._value == rhs._value

    __hash__ = PipIterable.__hash__

    def __len__(self):
        return len(self._value)

    def toNumber(self):
        # Returns a Python list containing Python number types
        return [item.toNumber() for item in self]

    def __contains__(self, item):
        return item in self._value

    def __getitem__(self, index):
        if isinstance(index, List):
            return List(self[i] for i in index)
        elif isinstance(index, Scalar):
            index = int(index)
        elif isinstance(index, Range):
            index = index.toSlice()
        
        if isinstance(index, int):
            if self.isEmpty():
                raise IndexError("Cannot index into empty list.")
            else:
                index %= len(self._value)
                return self._value[index]
        elif isinstance(index, slice):
            if self._value == []:
                # Slicing the empty list gives empty list
                return self
            length = len(self._value)
            repList = self._value[:]  # Shallow copy so as not to change _value
            start = index.start if index.start is not None else 0
            stop = index.stop if index.stop is not None else length
            if start < 0:
                start += length
            if stop < 0:
                stop += length
            if start >= stop:
                return List([])
            while start < 0:
                repList = self._value + repList
                start += length
                stop += length
            while stop > len(repList):
                repList += self._value
            return List(repList[start:stop])
        else:
            raise TypeError(f"Cannot use {type(index)} to index List")

    def __setitem__(self, index, item):
        if isinstance(index, int):
            index %= len(self._value)
        self._value.__setitem__(index, item)

    def __iter__(self):
        return iter(self._value)

    def count(self, item):
        return self._value.count(item)

    def append(self, item):
        # This assumes that item is an instance of a Pip type--unpredictable
        # behavior may follow if it's not
        self._value.append(item)

    def extend(self, iterable):
        # This assumes that iterable is either a Python list of Pip objects or
        # a List/Range/Scalar--unpredictable behavior otherwise
        self._value.extend(list(iterable))

    def index(self, searchItem, startIndex=0):
        try:
            return self._value.index(searchItem, startIndex)
        except ValueError:
            raise ValueError(f"{searchItem} is not in List at index "
                             f">= {startIndex}")


class Range(PipIterable):
    """Represents a range of integer values."""
    # TODO: add a step parameter

    def __init__(self, value, upperVal=None):
        if isinstance(value, Scalar):
            value = int(value)
        elif isinstance(value, (range, slice)):
            # Convert from Python range or slice object
            upperVal = value.stop
            value = value.start
        elif value is nil:
            value = None
        if isinstance(upperVal, Scalar):
            upperVal = int(upperVal)
        
        if isinstance(value, int) or value is None:
            if upperVal is None:
                # A single argument is actually the upper value
                self._lower = None
                self._upper = value
            elif isinstance(upperVal, (int, Nil)):
                self._lower = value
                self._upper = upperVal if upperVal is not nil else None
            else:
                raise TypeError(f"Cannot convert {type(upperVal)} to Range")
        else:
            raise TypeError(f"Cannot convert {type(value)} to Range")

    def copy(self):
        return Range(self._lower,
                     self._upper if self._upper is not None else nil)

    def getLower(self):
        return self._lower

    def getUpper(self):
        return self._upper

    def isEmpty(self):
        lower = self._lower or 0
        return self.isFinite() and self._upper <= lower

    def isFinite(self):
        return self._upper is not None

    def __str__(self):
        if self.isFinite():
            # Treat finite Ranges like Lists
            return str(List(self))
        else:
            # Infinite Ranges have to use the repr
            return repr(self)

    def __repr__(self):
        lower = self._lower if self._lower is not None else "()"
        upper = self._upper if self._upper is not None else "()"
        return f"({lower},{upper})"

    def __bool__(self):
        return not self.isEmpty()

    def __eq__(self, rhs):
        return (type(self) is type(rhs)
                and self._lower == rhs._lower
                and self._upper == rhs._upper)

    __hash__ = PipIterable.__hash__

    def __len__(self):
        lower = self._lower or 0
        if self.isFinite():
            return max(0, self._upper - lower)
        else:
            # A Range with no upper bound has an infinite length
            raise ValueError("Cannot take len() of infinite Range")

    def toNumber(self):
        # Returns a Python list containing Python numbers (probably ints)
        if self.isFinite():
            return [item.toNumber() for item in self]
        else:
            # TBD: possibly return a generator instead? Check contexts where
            # this is used
            return []

    def __contains__(self, item):
        # TBD: Should this return true only for ints, or for any number
        # between lower and upper?
        if isinstance(item, Scalar):
            lower = self._lower or 0
            if self.isFinite():
                return lower <= item.toNumber() < self._upper
            else:
                return lower <= item.toNumber()
        else:
            return False

    def toSlice(self):
        return slice(self._lower, self._upper)

    def toRange(self):
        if self.isFinite():
            # Treat lower value of None as 0
            lower = self._lower or 0
            return range(lower, self._upper)
        else:
            # Can't return an infinite range
            raise ValueError("Cannot convert infinite Range to Python range")

    def __iter__(self):
        isInfinite = not self.isFinite()
        if isInfinite:
            # TODO: use warning mechanism instead of print()?
            print("Iterating over an infinite Range", file=sys.stderr)
        i = self._lower or 0
        while isInfinite or i < self._upper:
            yield Scalar(i)
            i += 1

    def __getitem__(self, index):
        if isinstance(index, List):
            return List(self[i] for i in index)
        
        lower = self._lower or 0
        if isinstance(index, (Scalar, int)):
            # Get a single element of the Range
            index = int(index)
            if self.isEmpty():
                raise IndexError("Cannot index into empty range.")
            elif not self.isFinite() and index < 0:
                # Can't count from the end of an infinite Range
                # TODO: IndexError instead
                return nil
            if self.isFinite():
                # Indices wrap around for finite Ranges
                index %= len(self)
            return Scalar(lower + index)
        elif isinstance(index, (Range, slice)):
            # Get a slice of the Range
            if isinstance(index, Range):
                start, stop = index._lower, index._upper
            else:
                start, stop = index.start, index.stop
            if self.isEmpty():
                # Any slice of an empty Range will be empty
                return self
            elif self.isFinite():
                length = len(self)
                start = start if start is not None else 0
                stop = stop if stop is not None else length
                if start >= -length and stop <= length:
                    # Convert to Python range, slice it, and convert back
                    result = range(lower, self._upper)[start:stop]
                    return Range(result)
                else:
                    # One or both slice bounds are outside the size of the
                    # Range; convert to a List to do extended slicing
                    result = List(self)
                    return result[start:stop]
            else:
                if (start is not None and start < 0
                        or stop is not None and stop < 0):
                    # One of the indices is negative; can't count from the end
                    # of an infinite Range
                    # TODO: IndexError instead
                    return nil
                if start is None:
                    # Keep the bottom end of the Range the same
                    newLower = self._lower
                else:
                    # Move the bottom end of the Range up
                    newLower = lower + start
                if stop is None:
                    # Leave the Range unbounded above
                    newUpper = nil
                else:
                    # Bound the Range above
                    newUpper = lower + stop
                return Range(newLower, newUpper)
    
    def count(self, number):
        # 1 if number is in the Range, 0 otherwise
        return int(number in self)

    def index(self, searchItem, startIndex=0):
        if not isinstance(searchItem, Scalar):
            raise TypeError("Range.index requires Scalar argument, "
                            f"not {type(searchItem)}")
        elif searchItem in self[startIndex:]:
            # __contains__ returns true for floats, but index shouldn't
            if int(searchItem) == searchItem.toNumber():
                index = int(searchItem) - (self._lower or 0)
                return index
            else:
                raise ValueError("Range.index requires integer argument, "
                                 f"not {searchItem}")
        else:
            raise ValueError(f"{searchItem} is not in Range at index "
                             f">= {startIndex}")


class Block(PipType):
    """Represents a Pip function object."""

    def __init__(self, statements, returnExpr=None):
        self._statements = statements
        if returnExpr is None:
            self._returnExpr = nil
        else:
            self._returnExpr = returnExpr

    def copy(self):
        return Block(self._statements, self._returnExpr)

    def getStatements(self):
        return self._statements

    def getReturnExpr(self):
        return self._returnExpr

    def isExpr(self):
        return not self._statements

    def isEmpty(self):
        return not self._statements and self._returnExpr is nil

    def isFinite(self):
        return True
    
    def __str__(self):
        return repr(self)

    def __repr__(self):
        statements = self._statements + [self._returnExpr]
        return "{" + parsing.unparse(statements) + "}"

    def __bool__(self):
        return not self.isEmpty()

    def __eq__(self, rhs):
        return (isinstance(rhs, Block)
                and self._statements == rhs._statements
                and self._returnExpr == rhs._returnExpr)

    __hash__ = PipType.__hash__

    def toNumber(self):
        return 0


class Nil(PipType):
    """Represents the nil object."""
    
    instance = None

    def __new__(cls):
        # The __new__ function is used here so there's only ever one instance
        # of Nil
        if cls.instance is None:
            cls.instance = super().__new__(cls)
        return cls.instance

    def copy(self):
        # Not really a copy, but implemented for completeness
        return self

    def isEmpty(self):
        return True

    def isFinite(self):
        return True

    def __str__(self):
        return ""

    def __repr__(self):
        return "()"

    def __bool__(self):
        return False

    def __iter__(self):
        return iter([])

    def __eq__(self, rhs):
        return self is rhs

    __hash__ = PipType.__hash__

    def toNumber(self):
        return 0
    
    def __getitem__(self, index):
        return self


nil = Nil()


def toPipType(pyObj):
    if isinstance(pyObj, PipType):
        # The argument is already a Pip type, no need for conversion
        return pyObj
    elif isinstance(pyObj, (str, int, float, bool)):
        return Scalar(pyObj)
    elif isinstance(pyObj, (list, tuple, set, types.GeneratorType, map)):
        return List(pyObj)
    elif isinstance(pyObj, (range, slice)):
        return Range(pyObj)
    elif pyObj is None:
        return nil
    elif isinstance(pyObj, tokens.Token):
        token = pyObj
        tokenString = str(token)
        if isinstance(token, tokens.Number):
            # Numbers and nil need no further processing
            return Scalar(tokenString)
        elif isinstance(token, tokens.String):
            # Strip the double-quotes off a literal string
            return Scalar(tokenString[1:-1])
        elif isinstance(token, tokens.Pattern):
            # Strip off backticks and simplify \` inside
            # `\1\\\`2\\` -> \1\\`2\\
            rawPattern = tokenString[1:-1].replace("\\`", "`")
            return Pattern(rawPattern)
        elif isinstance(token, tokens.Char):
            # Single-quoted character
            return Scalar(tokenString[1])
        elif isinstance(token, tokens.EscapedString):
            # This will be an escaped string without any interpolations
            # Strip off \" delimiters and simplify backslash escapes
            return Scalar(tokenString[2:-2].replace(r"\\", "\\"))
        elif isinstance(token, tokens.Nil):
            return nil
        else:
            raise TypeError(f"Cannot convert {token!r} to Pip type")
    else:
        raise TypeError(f"Cannot convert {type(pyObj)} to Pip type")

