
import tokens

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
                 flags):
        super().__init__(token)
        self.function = function
        self.arity = arity
        self.precedence = precedence
        self.associativity = associativity
        self.flags = flags
        self.modifiesBlocks = flags & IN_LAMBDA
        self.assign = False  # Turns + into +: for instance
        self.fold = False  # Turns + into $+ for instance

    def __str__(self):
        return (("$" if self.fold else "")
                + self._text
                + (":" if self.assign else ""))

    def __repr__(self):
        return "Operator({},{},{},{},{})".format(str(self),
                                                 self.function,
                                                 self.arity,
                                                 self.precedence,
                                                 self.associativity)

    def copy(self):
        cpy = Operator(self,
                       self.function,
                       self.arity,
                       self.precedence,
                       self.associativity,
                       self.flags)
        cpy.assign = self.assign
        cpy.fold = self.fold
        return cpy

cmdTable = [
    ("F", "FOR", ["NAME", "RVAL", "CODE"]),
    ("I", "IF", ["RVAL", "CODE", "ELSE"]),
    ("L", "LOOP", ["RVAL", "CODE"]),
    ("O", "OUTPUT", ["RVAL"]),
    ("P", "PRINT", ["RVAL"]),
    ("Q", "QUERY", ["LVAL"]),
    ("S", "SWAP", ["LVAL", "LVAL"]),
    ("T", "TILL", ["RVAL", "CODE"]),
    ("W", "WHILE", ["RVAL", "CODE"]),
    ]

commands = {cmdSpecs[0]:Command(*cmdSpecs) for cmdSpecs in cmdTable}

# The precedence table contains a list of precedence levels from lowest to
# highest. The first entry in each level is the arity of all operators at that
# level. Each tuple that follows contains the operator symbol, the function,
# and optionally the associativity (default "L"). "C" associativity means
# "Chaining," used for comparison operators (e.g. 1<x<5).

# Flags for operator properties:
VALS = 0x01          # Evaluate all arguments (to lvals or rvals)
RVALS = 0x02         # Evaluate all arguments to rvals
RANGE_EACH = 0x04    # Convert Range arguments to equivalent List of ints
LIST_EACH = 0x08     # Perform operation item by item on Lists
IN_LAMBDA = 0x10     # Can be used to build lambda expressions from _

precedenceTable = [
    [2,
     (":", "ASSIGN", "R", VALS),
     ("M", "MAP", "R", RVALS),
     ],
    [3,
     ("?", "IFTE", "R"),
     ],
    [2,
     ("|", "OR", "L"),
     ],
    [2,
     ("&", "AND", "L"),
     ],
    [1,
     ("!", "NOT", "L", RVALS),
     ],
    [2,
     ("<", "NUMLESS", "C", RVALS),
     (">", "NUMGREATER", "C", RVALS),
     ("=", "NUMEQUAL", "C", RVALS),
     ("<=", "NUMLESSEQ", "C", RVALS),
     (">=", "NUMGREATEREQ", "C", RVALS),
     ("!=", "NUMNOTEQUAL", "C", RVALS),
     ("LT", "STRLESS", "C", RVALS),
     ("GT", "STRGREATER", "C", RVALS),
     ("EQ", "STREQUAL", "C", RVALS),
     ("LE", "STRLESSEQ", "C", RVALS),
     ("GE", "STRGREATEREQ", "C", RVALS),
     ("NE", "STRNOTEQUAL", "C", RVALS),
     ("==", "OBJEQUAL", "C", RVALS),
     ],
    [2,
     ("IN", "IN", "L", RVALS),
     ("NI", "NOTIN", "L", RVALS),
     ],
    [1,
     ("RP", "REPR", "L", RVALS),
     ("ST", "STR", "L", RVALS),
     ],
    [1,
     ("MX", "MAX", "L", RVALS),
     ("MN", "MIN", "L", RVALS),
     ("SN", "SORTNUM", "L", RVALS),
     ("UQ", "UNIQUE", "L", RVALS),
     ],
    [2,
     ("AE", "APPENDELEM", "L", RVALS),
     ("AL", "APPENDLIST", "L", RVALS),
     ("PE", "PREPENDELEM", "L", RVALS),
     ],
    [2,
     ("^", "SPLIT", "L", RVALS | LIST_EACH),
     ("<>", "GROUP", "L", RVALS),
     ("J", "JOIN", "L", RVALS),
     ("RL", "REPEATLIST", "L", RVALS),
     ],
    [1,
     ("^", "SPLIT", "L", RVALS | LIST_EACH),
     ("J", "JOIN", "L", RVALS),
     ],
    [3,
     ("R", "REPLACE", "L", RVALS),
     ],
    [2,
     (".", "CAT", "L", RVALS | RANGE_EACH | LIST_EACH),
     ],
    [2,
     ("RM", "REMOVE", "L", RVALS),
     ],
    [2,
     ("X", "STRMUL", "L", RVALS | RANGE_EACH | LIST_EACH),
     ],
    [1,
     ("RV", "REVERSE", "L", RVALS),
     ("LC", "LOWERCASE", "L", RVALS | LIST_EACH),
     ("UC", "UPPERCASE", "L", RVALS | LIST_EACH),
     ],
    [2,
     (",", "RANGE", "L", RVALS),
     ("RR", "RANDRANGE", "L", RVALS),
     ("TB", "TOBASE", "L", RVALS | RANGE_EACH | LIST_EACH),
     ],
    [1,
     (",", "RANGETO", "L", RVALS),
     ("RR", "RANDRANGETO", "L", RVALS),
     ("TB", "TOBASE", "L", RVALS | RANGE_EACH | LIST_EACH), # Unary mnemonic:
                                                            # ToBinary
     ],
    [2,
     ("<=>", "NUMCMP", "L", RVALS),
     ],
    [2,
     ("+", "ADD", "L", RVALS | LIST_EACH),
     ("-", "SUB", "L", RVALS | LIST_EACH),
     ],
    [2,
     ("*", "MUL", "L", RVALS | RANGE_EACH | LIST_EACH),
     ("/", "DIV", "L", RVALS | RANGE_EACH | LIST_EACH),
     ("%", "MOD", "L", RVALS | RANGE_EACH | LIST_EACH),
     ("//", "INTDIV", "L", RVALS | RANGE_EACH | LIST_EACH),
     ],
    [1,
     ("+", "POS", "L", RVALS | LIST_EACH),
     ("-", "NEG", "L", RVALS | LIST_EACH),
     ],
    [2,
     ("**", "POW", "R", RVALS | RANGE_EACH | LIST_EACH),
     ],
    [2,
     ("FB", "FROMBASE", "L", RVALS | RANGE_EACH | LIST_EACH),
     ],
    [2,
     ("@", "AT", "L", VALS),
     ("@<", "LEFTOF", "L", VALS),
     ("@>", "RIGHTOF", "L", VALS),
     ],
    [1,
     ("++", "INC", "L", VALS),
     ("--", "DEC", "L", VALS),
     ("#", "LEN", "L", RVALS),
     ("A", "ASC", "L", RVALS | RANGE_EACH | LIST_EACH),
     ("C", "CHR", "L", RVALS | RANGE_EACH | LIST_EACH),
     ("FB", "FROMBASE", "L", RVALS | RANGE_EACH | LIST_EACH), # Unary mnemonic:
                                                              # FromBinary
     ],
    ]

# Now take the information from the precedence table and rearrange it into
# structures more useful to the scanner and parser:

opsByArity = {1:{}, 2:{}, 3:{}}
operators = set()

# Some convenience operators for parsing that don't have straightforward
# equivalents in the syntax:
highestPrecedence = len(precedenceTable)
paren = Operator("PAREN", "PARENTHESIZE", 1, highestPrecedence, "L", VALS)
enlist = Operator("LIST", "LIST", 1, highestPrecedence, "L", RVALS)
block = Operator("BLOCK", "BLOCK", 1, highestPrecedence, "L", 0)
send = Operator("SEND", "SEND", 1, highestPrecedence, "L", VALS)
chain = None  # We define this "operator" later to make sure it gets the same
              # precedence as the comparison operators it comprises

for precedence, (arity, *entries) in enumerate(precedenceTable):
    for entry in entries:
        text, function = entry[:2]
        associativity = "L"
        flags = 0
        if len(entry) > 2:
            associativity = entry[2]
        if len(entry) > 3:
            flags = entry[3]
        if arity > 2 and flags & (LIST_EACH | RANGE_EACH):
            # TODO: proper implementation error message
            msg = "Current implementation cannot handle LIST_EACH or RANGE_EACH"
            msg += "\nfor operators of arity greater than 2 (like %s)" % text
            print(msg)
            flags = flags & ~LIST_EACH & ~RANGE_EACH
        op = Operator(text,
                      function,
                      arity,
                      precedence,
                      associativity,
                      flags)
        if associativity == "C" and not chain:
            # Define the CHAIN pseudo-operator, which allows parsing of
            # chained comparisons like 1<x<5
            chain = Operator("CHAIN", "CHAIN", 2, precedence, "C", 0)
        opsByArity[arity][text] = op
        operators.add(text)

