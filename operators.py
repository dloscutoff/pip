
import tokens, ptypes

class Command(tokens.Token):
    def __init__(self, token, function, argtypes):
        super().__init__(token)
        self.function = function
        self.argtypes = argtypes

    def __str__(self):
        return self._text

    def __repr__(self):
        return f"Command({self._text},{self.function},{self.argtypes})"

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
        try:
            self.default = ptypes.toPipType(default)
        except TypeError:
            print("Unsupported default value type for operator "
                  f"{token} ({function}):", type(default))
            self.default = ptypes.nil

    def __str__(self):
        opString = self._text
        if self.fold:
            opString = "$" + opString
        if self.map:
            if self._text + "*" in operators:
                # Add a space to mapped versions of operators like @
                # to prevent ambiguity with @* operator
                opString += " *"
            else:
                opString += "*"
        if self.assign:
            opString += ":"
        return opString

    def __repr__(self):
        return (f"Operator({self},{self.function},{self.arity},"
                f"{self.precedence},{self.associativity})")

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
# LOOPVAR - a single name, or multiple names in square braces
# EXPR - any expression
# CODE - a single statement, or a block of statements in curly braces
# ELSE - the E token followed by CODE
# WITH - the W token followed by EXPR

cmdTable = [
    ("F", "FOR", ["LOOPVAR", "EXPR", "CODE"]),
    ("I", "IF", ["EXPR", "CODE", "ELSE"]),
    ("L", "LOOP", ["EXPR", "CODE"]),
    ("LR", "LOOPREGEX", ["EXPR", "EXPR", "CODE"]),
    ("T", "TILL", ["EXPR", "CODE"]),
    ("W", "WHILE", ["EXPR", "CODE"]),
    ("WG", "WIPEGLOBALS", []),
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
     ("YO", "YANKOUTPUT", None, RVALS),
     ("YP", "YANKPRINT", None, RVALS),
     ],
    [2, "R",
     (":", "ASSIGN", None, VALS),
     ("::", "SWAP", None, VALS),
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
    [1, None,
     ("RE", "RECURSE", None, RVALS),
     ],
    [2, "R",
     ("M", "MAP", [], RVALS),
     ("MC", "MAPCOORDS", [], RVALS),
     ("ME", "MAPENUMERATE", [], RVALS),
     ("MJ", "MAPJOIN", "", RVALS),
     ("MM", "MAPMAP", [], RVALS),
     ("MP", "MAPPAIRS", [], RVALS),
     ("MS", "MAPSUM", 0, RVALS),
     ("MU", "MAPUNPACK", [], RVALS),
     ("FI", "FILTER", [], RVALS),
     ("SK", "SORTKEYED", [], RVALS),
     ("V", "EVAL", None, RVALS),
     ],
    [3, "R",
     ("MR", "MAPREGEX", [], RVALS),
     ("MZ", "MAPZIP", [], RVALS),
     ],
    [1, None,
     ("V", "EVAL", None, RVALS),
     ("FI", "FILTER", None, RVALS),
     ],
    [3, "R",
     (r"\?", "RVALIFTE", None, RVALS | IN_LAMBDA),
     ],
    [2, "L",
     (r"\|", "RVALOR", 0, RVALS | IN_LAMBDA),
     ],
    [2, "L",
     (r"\&", "RVALAND", 1, RVALS | IN_LAMBDA),
     ],
    [1, None,
     (r"\!", "NOT", None, RVALS | IN_LAMBDA),
     ],
    [2, "C",
     ("<", "NUMLESS", 1, RVALS | IN_LAMBDA),
     (">", "NUMGREATER", 1, RVALS | IN_LAMBDA),
     ("=", "NUMEQUAL", 1, RVALS | IN_LAMBDA),
     ("<=", "NUMLESSEQ", 1, RVALS | IN_LAMBDA),
     (">=", "NUMGREATEREQ", 1, RVALS | IN_LAMBDA),
     ("!=", "NUMNOTEQUAL", 1, RVALS | IN_LAMBDA),
     ("LT", "STRLESS", 1, RVALS | IN_LAMBDA),
     ("GT", "STRGREATER", 1, RVALS | IN_LAMBDA),
     ("Q", "STREQUAL", 1, RVALS | IN_LAMBDA),
     ("LE", "STRLESSEQ", 1, RVALS | IN_LAMBDA),
     ("GE", "STRGREATEREQ", 1, RVALS | IN_LAMBDA),
     ("NE", "STRNOTEQUAL", 1, RVALS | IN_LAMBDA),
     ("#=", "LENEQUAL", 1, RVALS | IN_LAMBDA),
     ("#<", "LENLESS", 1, RVALS | IN_LAMBDA),
     ("#>", "LENGREATER", 1, RVALS | IN_LAMBDA),
     ("~=", "FULLMATCH", 1, RVALS | IN_LAMBDA),
     ],
     # Note: comparison operators can be used in lambdas because the
     # CHAIN pseudo-operator has the IN_LAMBDA flag (see below). The
     # presence of the flag in this list has no actual effect.
    [2, "L",
     ("N", "IN", None, RVALS | IN_LAMBDA),
     ("NI", "NOTIN", None, RVALS | IN_LAMBDA),
     ],
    [1, None,
     ("RP", "REPR", None, RVALS),
     ("ST", "STR", None, RVALS),
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
     ("PK", "PICK", [], VALS | IN_LAMBDA),
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
     ("H", "PREFIX", [], VALS | IN_LAMBDA),  # Mnemonic: Head
     ("S", "SUFFIX", [], VALS | IN_LAMBDA),
     ("WV", "WEAVE", [], RVALS | IN_LAMBDA),
     ("UW", "UNWEAVE", [], RVALS | IN_LAMBDA),
     ("CP", "CARTESIANPRODUCT", [], RVALS | IN_LAMBDA),
     ("CG", "COORDINATEGRID", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("ZG", "ZEROGRID", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("OG", "ONEGRID", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("TD", "TODIGITS", [], RVALS | IN_LAMBDA ),
     ("FD", "FROMDIGITS", 0, RVALS | IN_LAMBDA ),
     ],
    [3, "L",
     ("RA", "REPLACEAT", None, RVALS | IN_LAMBDA),
     ("TR", "TRANSLITERATE", None, RVALS | IN_LAMBDA),
     ],
    [1, None,
     ("^", "SPLIT", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("<>", "GROUP", None, RVALS | IN_LAMBDA),
     ("J", "JOIN", None, RVALS | IN_LAMBDA),
     ("RV", "REVERSE", None, RVALS | IN_LAMBDA),
     ("R", "REVERSE", None, RVALS | IN_LAMBDA), # Synonym for golfiness
     ("RF", "REFLECT", None, RVALS | IN_LAMBDA),
     ("PZ", "PALINDROMIZE", None, RVALS | IN_LAMBDA),
     ("QR", "QUADREFLECT", None, RVALS | IN_LAMBDA),
     ("QP", "QUADPALINDROMIZE", None, RVALS | IN_LAMBDA),
     ("Z", "ZIP", None, RVALS | IN_LAMBDA),
     ("ZD", "ZIPDEFAULT", None, RVALS | IN_LAMBDA),
     ("H", "PREFIX", None, VALS | IN_LAMBDA),  # Mnemonic: Head
     ("S", "SUFFIX", None, VALS | IN_LAMBDA),
     ("WV", "WEAVE", None, RVALS | IN_LAMBDA),
     ("UW", "UNWEAVE", None, RVALS | IN_LAMBDA),
     ("CP", "CARTESIANPRODUCT", None, RVALS | IN_LAMBDA),
     ("CG", "COORDINATEGRID", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("ZG", "ZEROGRID", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("OG", "ONEGRID", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("EY", "IDENTITYMATRIX", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("TD", "TODIGITS", None, RVALS | IN_LAMBDA ),
     ("FD", "FROMDIGITS", None, RVALS | IN_LAMBDA ),
     ],
    [3, "L",
     ("R", "REPLACE", None, RVALS),
     ],
    [2, "L",
     ("WR", "WRAP", "", RVALS | IN_LAMBDA | RANGE_EACH),
     ("~", "FIRSTMATCH", "", RVALS | IN_LAMBDA | RANGE_EACH),
     ],
    [2, "L",
     (".", "CAT", "", RVALS | IN_LAMBDA | RANGE_EACH),
     ],
    [1, None,
     ("X", "REGEX", None, RVALS | IN_LAMBDA),
     (".", "DOT", None, RVALS | IN_LAMBDA | LIST_EACH),
     ("K", "KLEENESTAR", None, RVALS | IN_LAMBDA | LIST_EACH),
     ],
    [2, "L",
     ("RM", "REMOVE", "", RVALS | IN_LAMBDA),
     ("DC", "DELETECHARS", "", RVALS | IN_LAMBDA | RANGE_EACH),
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
     ("SC", "SWAPCASE", None, RVALS | IN_LAMBDA | LIST_EACH),
     ],
    [2, "L",
     (",", "RANGE", None, RVALS | IN_LAMBDA | RANGE_EACH),
     (r"\,", "INCLRANGE", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("RR", "RANDRANGE", 0, RVALS | IN_LAMBDA | RANGE_EACH),
     ("TB", "TOBASE", 0, RVALS | IN_LAMBDA | RANGE_EACH),
     ],
    [1, None,
     (",", "RANGETO", None, RVALS | IN_LAMBDA | RANGE_EACH),
     (r"\,", "INCLRANGETO", None, RVALS | IN_LAMBDA | RANGE_EACH),
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
     ("AD", "ABSOLUTEDIFF", 0, RVALS | IN_LAMBDA | RANGE_EACH),
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
     ("%", "MOD", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ],
    [2, "R",
     ("**", "POW", 1, RVALS | IN_LAMBDA | RANGE_EACH),
     ("E", "POW", 1, RVALS | IN_LAMBDA | RANGE_EACH),
     ("EE", "POWEROFTEN", 1, RVALS | IN_LAMBDA | RANGE_EACH),
     ("RT", "ROOT", 1, RVALS | IN_LAMBDA | RANGE_EACH),
     ],
    [1, None,
     ("**", "POW", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("E", "POW", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("EE", "POWEROFTEN", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("RT", "SQRT", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("SQ", "SQUARE", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("HV", "HALVE", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("DB", "DOUBLE", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("SI", "SINE", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("CO", "COSINE", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("TA", "TANGENT", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("SE", "SECANT", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("CS", "COSEC", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("CT", "COTAN", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("AT", "ARCTAN", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("RD", "RADIANS", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("DG", "DEGREES", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("EX", "EXPONENTIAL", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("LN", "NATURALLOG", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ],
    [2, "L",
     ("FB", "FROMBASE", 0, RVALS | IN_LAMBDA | RANGE_EACH),
     ],
    [1, None,
     ("U", "INC", None, VALS | IN_LAMBDA),
     ("D", "DEC", None, VALS | IN_LAMBDA),
     ("#", "LEN", None, RVALS | IN_LAMBDA),
     ("A", "ASC", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("C", "CHR", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("AB", "ABS", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("SG", "SIGN", None, RVALS | IN_LAMBDA | RANGE_EACH),
     ("FB", "FROMBASE", None, RVALS | IN_LAMBDA | RANGE_EACH),
     # Unary mnemonic: FromBinary
     ],
    [2, "L",
     ("@", "AT", None, VALS | IN_LAMBDA),
     ("@<", "LEFTOF", None, VALS | IN_LAMBDA),
     ("@>", "RIGHTOF", None, VALS | IN_LAMBDA),
     ],
    [1, None,
     ("@", "AT", None, VALS | IN_LAMBDA),
     ("@<", "LEFTOF", None, VALS | IN_LAMBDA),
     ("@>", "RIGHTOF", None, VALS | IN_LAMBDA),
     ("++", "INC", None, VALS | IN_LAMBDA),
     ("--", "DEC", None, VALS | IN_LAMBDA),
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
enlist = Operator("LIST", "LIST", 1, highestPrecedence, "L", None, VALS)
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
                   f"(like {text})")
            print(msg)
            flags = flags & ~LIST_EACH & ~RANGE_EACH
        if flags & IN_LAMBDA and not flags & (VALS | RVALS):
            # TODO: proper implementation error message
            msg = ("IN_LAMBDA may not be set for operators that do not "
                   f"also set VALS or RVALS\n ({text})")
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
