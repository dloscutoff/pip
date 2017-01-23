
import tokens, ptypes

class Command(tokens.Token):
    def __init__(self, token, function, argtypes):
        super().__init__(token)
        self.function = function
        self.argtypes = argtypes

    def __str__(self):
        return self._text

    def __repr__(self):
        return "Command({},{},{})".format(self._text,
                                          self.function,
                                          self.argtypes)

class Operator(tokens.Token):
    def __init__(self, token, function, arity, precedence, associativity,
                 default=None, flags=0):
        super().__init__(token)
        self.function = function
        self.arity = arity
        self.precedence = precedence
        self.associativity = associativity
        self.flags = flags
        self.assign = False  # Turns + into +: for instance
        self.map = False  # Turns ! into !* for instance
        self.fold = False  # Turns + into $+ for instance

        # The default argument represents the value when folding an empty list
        # with this operator. For economy of space, defaults are specified as
        # Python objects in the operator table; convert them to Pip types here.
        if type(default) in (ptypes.Nil, ptypes.Scalar, ptypes.List):
            self.default = default
        elif default is None:
            self.default = ptypes.nil
        elif type(default) in (int, float, str):
            self.default = ptypes.Scalar(default)
        elif type(default) is list:
            self.default = ptypes.List(default)
        else:
            print("Unsupported operator default value type:", type(default))
            self.default = ptypes.nil

    def __str__(self):
        return (("$" if self.fold else "")
                + self._text
                + ("*" if self.map else "")
                + (":" if self.assign else ""))

    def __repr__(self):
        return "Operator({},{},{},{},{})".format(str(self),
                                                 self.function,
                                                 self.arity,
                                                 self.precedence,
                                                 self.associativity)

    def copy(self):
        cpy = Operator(self._text,
                       self.function,
                       self.arity,
                       self.precedence,
                       self.associativity,
                       self.default,
                       self.flags)
        cpy.assign = self.assign
        cpy.fold = self.fold
        cpy.map = self.map
        return cpy


# Each entry in the command table contains the command symbol, the function,
# and a list of parsing items that the command expects:
# NAME - a single variable name
# NAMES - one or more variable names
# EXPR - any expression
# CODE - a single statement, or a block of statements in curly braces
# ELSE - the E token followed by CODE
# WITH - the W token followed by EXPR

cmdTable = [
    ("F", "FOR", ["NAME", "EXPR", "CODE"]),
    ("I", "IF", ["EXPR", "CODE", "ELSE"]),
    ("L", "LOOP", ["EXPR", "CODE"]),
    ("S", "SWAP", ["EXPR", "EXPR"]),
    ("T", "TILL", ["EXPR", "CODE"]),
    ("U", "UNIFY", ["NAMES", "WITH"]),
    ("W", "WHILE", ["EXPR", "CODE"]),
    ]

commands = {cmdSpecs[0]:Command(*cmdSpecs) for cmdSpecs in cmdTable}

# The precedence table contains a list of precedence levels from lowest to
# highest. The first two entries in each level are the arity and associativity
# of all operators at that level. "C" associativity means "Chaining," used for
# comparison operators (e.g. 1<x<5). Each tuple that follows contains the
# operator symbol, the function, the default value if used to fold an empty
# iterable, and optionally some flags.

# Flags for operator properties:
VALS = 0x01          # Evaluate all arguments (to lvals or rvals)
RVALS = 0x02         # Evaluate all arguments to rvals
LIST_EACH = 0x04     # Perform operation item by item on Lists
RANGE_EACH = 0x08    # Convert Range arguments to equivalent List of ints;
                     # implies LIST_EACH
IN_LAMBDA = 0x10     # Can be used to build lambda expressions from _

precedenceTable = [
    [1, None,
     ("O", "OUTPUT", None, RVALS),
     ("P", "PRINT", None, RVALS),
     ("Y", "YANK", None, RVALS),
     ],
    [2, "R",
     (":", "ASSIGN", None, VALS),
     ],
    [3, "R",
     ("?", "IFTE", None, ),
     ],
    [2, "L",
     ("|", "OR", 0, ),
     ],
    [2, "L",
     ("&", "AND", 1, ),
     ],
    [1, None,
     ("!", "NOT", None, RVALS),
     ],
    [2, "L",
     ("==", "OBJEQUAL", 1, RVALS),  # NB: *not* a chaining operator!
     ],
    [2, "R",
     ("M", "MAP", [], RVALS),
     ("MM", "MAPMAP", [], RVALS),
     ("MJ", "MAPJOIN", "", RVALS),
     ("MS", "MAPSUM", 0, RVALS),
     ("MU", "MAPUNPACK", [], RVALS),
     ("MP", "MAPPAIRS", [], RVALS),
     ("MC", "MAPCOORDS", [], RVALS),
     ("FI", "FILTER", [], RVALS),
     ("SK", "SORTKEYED", [], RVALS),
     ("V", "EVAL", None, RVALS),
     ],
    [3, "R",
     ("MZ", "MAPZIP", [], RVALS),
     ],
    [1, None,
     ("V", "EVAL", None, RVALS),
     ],
    [2, "C",
     ("<", "NUMLESS", 1, RVALS),
     (">", "NUMGREATER", 1, RVALS),
     ("=", "NUMEQUAL", 1, RVALS),
     ("<=", "NUMLESSEQ", 1, RVALS),
     (">=", "NUMGREATEREQ", 1, RVALS),
     ("!=", "NUMNOTEQUAL", 1, RVALS),
     ("LT", "STRLESS", 1, RVALS),
     ("GT", "STRGREATER", 1, RVALS),
     ("Q", "STREQUAL", 1, RVALS),
     ("EQ", "STREQUAL", 1, RVALS),  # Synonym for backward compatibility
     ("LE", "STRLESSEQ", 1, RVALS),
     ("GE", "STRGREATEREQ", 1, RVALS),
     ("NE", "STRNOTEQUAL", 1, RVALS),
     ("#=", "LENEQUAL", 1, RVALS),
     ("#<", "LENLESS", 1, RVALS),
     ("#>", "LENGREATER", 1, RVALS),
     ],
     # Note: comparison operators CAN also be used in lambdas, due to the
     # CHAIN pseudo-operator having the IN_LAMBDA flag (see below).
    [2, "L",
     ("N", "IN", None, RVALS | IN_LAMBDA),
     ("IN", "IN", None, RVALS | IN_LAMBDA), # Synonym for backward compat
     ("NI", "NOTIN", None, RVALS | IN_LAMBDA),
     ],
    [1, None,
     ("RP", "REPR", None, RVALS),
     ("ST", "STR", None, RVALS),
     ("X", "REGEX", None, RVALS | IN_LAMBDA),
     ],
    [2, "L",
     ("CB", "COMBINATIONS", [], RVALS | IN_LAMBDA),
     ],
    [1, None,
     ("MX", "MAX", None, RVALS | IN_LAMBDA),
     ("MN", "MIN", None, RVALS | IN_LAMBDA),
     ("RC", "RANDCHOICE", None, RVALS | IN_LAMBDA),
     ("SN", "SORTNUM", None, RVALS | IN_LAMBDA),
     ("SS", "SORTSTRING", None, RVALS | IN_LAMBDA),
     ("UQ", "UNIQUE", None, RVALS | IN_LAMBDA),
     ("EN", "ENUMERATE", None, RVALS | IN_LAMBDA),
     ("PM", "PERMUTATIONS", None, RVALS | IN_LAMBDA),
     ],
    [2, "L",
     ("AE", "APPENDELEM", [], RVALS | IN_LAMBDA),
     ("AL", "APPENDLIST", [], RVALS | IN_LAMBDA),
     ("PE", "PREPENDELEM", [], RVALS | IN_LAMBDA),
     ("PU", "PUSH", [], VALS | IN_LAMBDA),
     ("PB", "PUSHBACK", [], VALS | IN_LAMBDA),
     ],
    [1, None,
     ("PO", "POP", None, VALS | IN_LAMBDA),
     ("DQ", "DEQUEUE", None, VALS | IN_LAMBDA),
     ],
    [2, "L",
     ("^", "SPLIT", [], RVALS | IN_LAMBDA | RANGE_EACH),
     ("^@", "SPLITAT", [], RVALS | IN_LAMBDA),
     ("@?", "FIND", None, RVALS | IN_LAMBDA),
     ("@*", "FINDALL", [], RVALS | IN_LAMBDA),
     ("<>", "GROUP", [], RVALS | IN_LAMBDA),
     ("J", "JOIN", "", RVALS | IN_LAMBDA),
     ("JW", "JOINWRAP", None, RVALS | IN_LAMBDA),
     ("RL", "REPEATLIST", [], RVALS | IN_LAMBDA),
     ("Z", "ZIP", [], RVALS | IN_LAMBDA),
     ("ZD", "ZIPDEFAULT", [], RVALS | IN_LAMBDA),
     ("WV", "WEAVE", [], RVALS | IN_LAMBDA),
     ("CP", "CARTESIANPRODUCT", [], RVALS | IN_LAMBDA),
     ("CG", "COORDINATEGRID", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("ZG", "ZEROGRID", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ],
    [3, "L",
     ("RA", "REPLACEAT", None, RVALS | IN_LAMBDA),
     ],
    [1, None,
     ("^", "SPLIT", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("J", "JOIN", None, RVALS | IN_LAMBDA),
     ("RV", "REVERSE", None, RVALS | IN_LAMBDA),
     ("Z", "ZIP", None, RVALS | IN_LAMBDA),
     ("ZD", "ZIPDEFAULT", None, RVALS | IN_LAMBDA),
     ("WV", "WEAVE", None, RVALS | IN_LAMBDA),
     ("CP", "CARTESIANPRODUCT", None, RVALS | IN_LAMBDA),
     ("CG", "COORDINATEGRID", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("ZG", "ZEROGRID", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("EY", "IDENTITYMATRIX", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ],
    [3, "L",
     ("R", "REPLACE", None, RVALS),
     ],
    [2, "L",
     ("WR", "WRAP", "", RVALS | IN_LAMBDA),
     ],
    [2, "L",
     (".", "CAT", "", RVALS | IN_LAMBDA | RANGE_EACH),
     ],
    [1, None,
     (".", "DOT", None, RVALS | IN_LAMBDA | LIST_EACH),
     ("K", "KLEENESTAR", None, RVALS | IN_LAMBDA | LIST_EACH),
     ],
    [2, "L",
     ("RM", "REMOVE", "", RVALS | IN_LAMBDA),
     ],
    [2, "L",
     ("X", "STRMUL", "", RVALS | IN_LAMBDA | RANGE_EACH),
     ],
    [2, "L",
     ("||", "STRIP", "", RVALS | IN_LAMBDA | LIST_EACH),
     ("|>", "LSTRIP", "", RVALS | IN_LAMBDA | LIST_EACH),
     ("<|", "RSTRIP", "", RVALS | IN_LAMBDA | LIST_EACH),
     ("TM", "TRIM", "", RVALS | IN_LAMBDA | LIST_EACH),
     ],
    [1, None,
     ("||", "STRIP", None, RVALS | IN_LAMBDA | LIST_EACH),
     ("|>", "LSTRIP", None, RVALS | IN_LAMBDA | LIST_EACH),
     ("<|", "RSTRIP", None, RVALS | IN_LAMBDA | LIST_EACH),
     ("TM", "TRIM", None, RVALS | IN_LAMBDA | LIST_EACH),
     ("LC", "LOWERCASE", None, RVALS | IN_LAMBDA | LIST_EACH),
     ("UC", "UPPERCASE", None, RVALS | IN_LAMBDA | LIST_EACH),
     ],
    [2, "L",
     (",", "RANGE", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("RR", "RANDRANGE", 0, RVALS | IN_LAMBDA | RANGE_EACH),
     ("TB", "TOBASE", 0, RVALS | IN_LAMBDA | RANGE_EACH),
     ],
    [1, None,
     (",", "RANGETO", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("RR", "RANDRANGETO", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("TB", "TOBASE", None, RVALS | IN_LAMBDA | RANGE_EACH),
     # Unary mnemonic: ToBinary
     ],
    [2, "L",
     ("BA", "BITWISEAND", -1, RVALS | IN_LAMBDA | RANGE_EACH),
     ("BO", "BITWISEOR", 0, RVALS | IN_LAMBDA | RANGE_EACH),
     ("BX", "BITWISEXOR", 0, RVALS | IN_LAMBDA | RANGE_EACH),
     ("AT", "ARCTAN", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("CM", "NUMCMP", 0, RVALS | IN_LAMBDA),
     ],
    [1, None,
     ("BN", "BITWISENOT", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ],
    [2, "L",
     ("+", "ADD", 0, RVALS | IN_LAMBDA | LIST_EACH),
     ("-", "SUB", 0, RVALS | IN_LAMBDA | LIST_EACH),
     ],
    [2, "L",
     ("*", "MUL", 1, RVALS | IN_LAMBDA | RANGE_EACH),
     ("/", "DIV", 1, RVALS | IN_LAMBDA | RANGE_EACH),
     ("%", "MOD", 0, RVALS | IN_LAMBDA | RANGE_EACH),
     ("//", "INTDIV", 1, RVALS | IN_LAMBDA | RANGE_EACH),
     ],
    [1, None,
     ("+", "POS", None, RVALS | IN_LAMBDA | LIST_EACH),
     ("-", "NEG", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("/", "INVERT", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ],
    [2, "R",
     ("**", "POW", 1, RVALS | IN_LAMBDA | RANGE_EACH),
     ("RT", "ROOT", 1, RVALS | IN_LAMBDA | RANGE_EACH),
     ],
    [1, None,
     ("RT", "SQRT", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("SI", "SINE", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("CO", "COSINE", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("TA", "TANGENT", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("SE", "SECANT", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("CS", "COSEC", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("CT", "COTAN", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("AT", "ARCTAN", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("RD", "RADIANS", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("DG", "DEGREES", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ],
    [2, "L",
     ("FB", "FROMBASE", 0, RVALS | IN_LAMBDA | RANGE_EACH),
     ],
    [2, "L",
     ("@", "AT", None, VALS | IN_LAMBDA),
     ("@<", "LEFTOF", None, VALS | IN_LAMBDA),
     ("@>", "RIGHTOF", None, VALS | IN_LAMBDA),
     ],
    [1, None,
     ("@<", "LEFTOF", None, VALS | IN_LAMBDA),
     ("@>", "RIGHTOF", None, VALS | IN_LAMBDA),
     ("++", "INC", None, VALS | IN_LAMBDA),
     ("--", "DEC", None, VALS | IN_LAMBDA),
     ("#", "LEN", None, RVALS | IN_LAMBDA),
     ("A", "ASC", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("C", "CHR", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("AB", "ABS", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("SG", "SIGN", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("FB", "FROMBASE", None, RVALS | IN_LAMBDA | RANGE_EACH),
     # Unary mnemonic: FromBinary
     ],
    ]

# Now take the information from the precedence table and rearrange it into
# structures more useful to the scanner and parser:

opsByArity = {1:{}, 2:{}, 3:{}}
operators = set()

# Some convenience operators for parsing that don't have straightforward
# equivalents in the syntax:
highestPrecedence = len(precedenceTable)
paren = Operator("PAREN", "PARENTHESIZE", 1,
                 highestPrecedence, "L", None, VALS)
enlist = Operator("LIST", "LIST", 1, highestPrecedence, "L", None, RVALS)
block = Operator("BLOCK", "BLOCK", 1, highestPrecedence, "L", None)
send = Operator("SEND", "SEND", 1, highestPrecedence, "L", None, VALS)
chain = None  # We define this "operator" later to make sure it gets the same
              # precedence as the comparison operators it comprises

for precedence, (arity, associativity, *entries) in enumerate(precedenceTable):
    for entry in entries:
        text, function, default = entry[:3]
        if len(entry) > 3:
            flags = entry[3]
        else:
            flags = 0
        if flags & RANGE_EACH:
            # RANGE_EACH implies LIST_EACH (though not vice versa)
            flags |= LIST_EACH
        if arity > 2 and flags & LIST_EACH:
            # TODO: proper implementation error message
            msg = ("Current implementation cannot handle LIST_EACH or "
                   "RANGE_EACH for operators\n of arity greater than 2 "
                   "(like %s)" % text)
            print(msg)
            flags = flags & ~LIST_EACH & ~RANGE_EACH
        if flags & IN_LAMBDA and not flags & (VALS | RVALS):
            # TODO: proper implementation error message
            msg = "IN_LAMBDA may not be set for operators that do not also"
            msg += "set VALS or RVALS\n (%s)" % text
            print(msg)
            flags = flags & ~IN_LAMBDA
        op = Operator(text,
                      function,
                      arity,
                      precedence,
                      associativity,
                      default,
                      flags)
        if associativity == "C" and not chain:
            # Define the CHAIN pseudo-operator, which allows parsing of
            # chained comparisons like 1<x<5
            chain = Operator("CHAIN",
                             "CHAIN",
                             2,
                             precedence,
                             "C",
                             None,
                             RVALS | IN_LAMBDA)
        opsByArity[arity][text] = op
        operators.add(text)
