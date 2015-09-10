
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
                       self.default,
                       self.flags)
        cpy.assign = self.assign
        cpy.fold = self.fold
        return cpy

cmdTable = [
    ("F", "FOR", ["NAME", "EXPR", "CODE"]),
    ("I", "IF", ["EXPR", "CODE", "ELSE"]),
    ("L", "LOOP", ["EXPR", "CODE"]),
    #("O", "OUTPUT", ["EXPR"]),
    #("P", "PRINT", ["EXPR"]),
    #("Q", "QUERY", ["EXPR"]),
    ("S", "SWAP", ["EXPR", "EXPR"]),
    ("T", "TILL", ["EXPR", "CODE"]),
    ("U", "UNIFY", ["NAMES", "WITH"]),
    ("W", "WHILE", ["EXPR", "CODE"]),
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
    [1,
     ("O", "OUTPUT", "L", None, RVALS),
     ("P", "PRINT", "L", None, RVALS),
     ("Y", "YANK", "L", None, RVALS),
     ],
    [2,
     (":", "ASSIGN", "R", None, VALS),
     ],
    [3,
     ("?", "IFTE", "R"),
     ],
    [2,
     ("|", "OR", "L", 0),
     ],
    [2,
     ("&", "AND", "L", 1),
     ],
    [1,
     ("!", "NOT", "L", None, RVALS),
     ],
    [2,
     ("==", "OBJEQUAL", "L", 1, RVALS),  # NB: *not* a chaining operator!
     ],
    [2,
     ("M", "MAP", "R", [], RVALS),
     ("MJ", "MAPJOIN", "R", "", RVALS),
     ("FI", "FILTER", "R", [], RVALS),
     ("V", "EVAL", "R", None, RVALS),
     ],
    [1,
     ("V", "EVAL", "L", None, RVALS),
     ],
    [2,
     ("<", "NUMLESS", "C", 1, RVALS),
     (">", "NUMGREATER", "C", 1, RVALS),
     ("=", "NUMEQUAL", "C", 1, RVALS),
     ("<=", "NUMLESSEQ", "C", 1, RVALS),
     (">=", "NUMGREATEREQ", "C", 1, RVALS),
     ("!=", "NUMNOTEQUAL", "C", 1, RVALS),
     ("LT", "STRLESS", "C", 1, RVALS),
     ("GT", "STRGREATER", "C", 1, RVALS),
     ("Q", "STREQUAL", "C", 1, RVALS),
     ("EQ", "STREQUAL", "C", 1, RVALS),  # Synonym for backward compatibility
     ("LE", "STRLESSEQ", "C", 1, RVALS),
     ("GE", "STRGREATEREQ", "C", 1, RVALS),
     ("NE", "STRNOTEQUAL", "C", 1, RVALS),
     ("#=", "LENEQUAL", "C", 1, RVALS),
     ("#<", "LENLESS", "C", 1, RVALS),
     ("#>", "LENGREATER", "C", 1, RVALS),
     ],
     # Note: comparison operators CAN also be used in lambdas, due to the
     # CHAIN pseudo-operator having the IN_LAMBDA flag (see below).
    [2,
     ("N", "IN", "L", None, RVALS | IN_LAMBDA),
     ("IN", "IN", "L", None, RVALS | IN_LAMBDA), # Synonym for backward compat
     ("NI", "NOTIN", "L", None, RVALS | IN_LAMBDA),
     ],
    [1,
     ("RP", "REPR", "L", None, RVALS),
     ("ST", "STR", "L", None, RVALS),
     ("X", "REGEX", "L", None, RVALS | IN_LAMBDA),
     ],
    [1,
     ("MX", "MAX", "L", None, RVALS | IN_LAMBDA),
     ("MN", "MIN", "L", None, RVALS | IN_LAMBDA),
     ("RC", "RANDCHOICE", "L", None, RVALS | IN_LAMBDA),
     ("SN", "SORTNUM", "L", None, RVALS | IN_LAMBDA),
     ("SS", "SORTSTRING", "L", None, RVALS | IN_LAMBDA),
     ("UQ", "UNIQUE", "L", None, RVALS | IN_LAMBDA),
     ("EN", "ENUMERATE", "L", None, RVALS | IN_LAMBDA),
     ],
    [2,
     ("AE", "APPENDELEM", "L", [], RVALS | IN_LAMBDA),
     ("AL", "APPENDLIST", "L", [], RVALS | IN_LAMBDA),
     ("PE", "PREPENDELEM", "L", [], RVALS | IN_LAMBDA),
     ("PU", "PUSH", "L", [], VALS | IN_LAMBDA),
     ("PB", "PUSHBACK", "L", [], VALS | IN_LAMBDA),
     ],
    [1,
     ("PO", "POP", "L", None, VALS | IN_LAMBDA),
     ("DQ", "DEQUEUE", "L", None, VALS | IN_LAMBDA),
     ],
    [2,
     ("^", "SPLIT", "L", [], RVALS | IN_LAMBDA | LIST_EACH),
     ("^@", "SPLITAT", "L", [], RVALS | IN_LAMBDA),
     ("@?", "FIND", "L", None, RVALS | IN_LAMBDA),
     ("@*", "FINDALL", "L", [], RVALS | IN_LAMBDA),
     ("<>", "GROUP", "L", [], RVALS | IN_LAMBDA),
     ("J", "JOIN", "L", "", RVALS | IN_LAMBDA),
     ("RL", "REPEATLIST", "L", [], RVALS | IN_LAMBDA),
     ("Z", "ZIP", "L", [], RVALS | IN_LAMBDA),
     ("ZD", "ZIPDEFAULT", "L", [], RVALS | IN_LAMBDA),
     ("CP", "CARTESIANPRODUCT", "L", [], RVALS | IN_LAMBDA),
     ("CG", "COORDINATEGRID", "L", None, RVALS | IN_LAMBDA),
     ("ZG", "ZEROGRID", "L", None, RVALS | IN_LAMBDA),
     ],
    [1,
     ("^", "SPLIT", "L", None, RVALS | IN_LAMBDA | LIST_EACH),
     ("J", "JOIN", "L", None, RVALS | IN_LAMBDA),
     ("RV", "REVERSE", "L", None, RVALS | IN_LAMBDA),
     ("Z", "ZIP", "L", None, RVALS | IN_LAMBDA),
     ("ZD", "ZIPDEFAULT", "L", None, RVALS | IN_LAMBDA),
     ("CP", "CARTESIANPRODUCT", "L", None, RVALS | IN_LAMBDA),
     ],
    [3,
     ("R", "REPLACE", "L", None, RVALS | IN_LAMBDA),
     ],
    [2,
     (".", "CAT", "L", "", RVALS | IN_LAMBDA | RANGE_EACH | LIST_EACH),
     ],
    [2,
     ("RM", "REMOVE", "L", "", RVALS | IN_LAMBDA),
     ],
    [2,
     ("X", "STRMUL", "L", "", RVALS | IN_LAMBDA | RANGE_EACH | LIST_EACH),
     ],
    [2,
     ("||", "STRIP", "L", "", RVALS | IN_LAMBDA),
     ("|>", "LSTRIP", "L", "", RVALS | IN_LAMBDA),
     ("<|", "RSTRIP", "L", "", RVALS | IN_LAMBDA),
     ("TM", "TRIM", "L", "", RVALS | IN_LAMBDA | LIST_EACH),
     ],
    [1,
     ("||", "STRIP", "L", "", RVALS | IN_LAMBDA | LIST_EACH),
     ("|>", "LSTRIP", "L", "", RVALS | IN_LAMBDA | LIST_EACH),
     ("<|", "RSTRIP", "L", "", RVALS | IN_LAMBDA | LIST_EACH),
     ("TM", "TRIM", "L", "", RVALS | IN_LAMBDA | LIST_EACH),
     ("LC", "LOWERCASE", "L", None, RVALS | IN_LAMBDA | LIST_EACH),
     ("UC", "UPPERCASE", "L", None, RVALS | IN_LAMBDA | LIST_EACH),
     ],
    [2,
     (",", "RANGE", "L", None, RVALS | IN_LAMBDA),
     ("RR", "RANDRANGE", "L", 0, RVALS | IN_LAMBDA),
     ("TB", "TOBASE", "L", 0, RVALS | IN_LAMBDA | RANGE_EACH | LIST_EACH),
     ],
    [1,
     (",", "RANGETO", "L", None, RVALS | IN_LAMBDA),
     ("RR", "RANDRANGETO", "L", None, RVALS | IN_LAMBDA),
     ("TB", "TOBASE", "L", None, RVALS | IN_LAMBDA | RANGE_EACH | LIST_EACH),
     # Unary mnemonic: ToBinary
     ],
    [2,
     ("BA", "BITWISEAND", "L", -1, RVALS | IN_LAMBDA | RANGE_EACH | LIST_EACH),
     ("BO", "BITWISEOR", "L", 0, RVALS | IN_LAMBDA | RANGE_EACH | LIST_EACH),
     ("BX", "BITWISEXOR", "L", 0, RVALS | IN_LAMBDA | RANGE_EACH | LIST_EACH),
     ],
    [1,
     ("BN", "BITWISENOT", "L",None, RVALS | IN_LAMBDA | RANGE_EACH | LIST_EACH),
     ],
    [2,
     ("<=>", "NUMCMP", "L", 0, RVALS | IN_LAMBDA),
     ],
    [2,
     ("+", "ADD", "L", 0, RVALS | IN_LAMBDA | LIST_EACH),
     ("-", "SUB", "L", 0, RVALS | IN_LAMBDA | LIST_EACH),
     ],
    [2,
     ("*", "MUL", "L", 1, RVALS | IN_LAMBDA | RANGE_EACH | LIST_EACH),
     ("/", "DIV", "L", 1, RVALS | IN_LAMBDA | RANGE_EACH | LIST_EACH),
     ("%", "MOD", "L", 0, RVALS | IN_LAMBDA | RANGE_EACH | LIST_EACH),
     ("//", "INTDIV", "L", 1, RVALS | IN_LAMBDA | RANGE_EACH | LIST_EACH),
     ],
    [1,
     ("+", "POS", "L", None, RVALS | IN_LAMBDA | LIST_EACH),
     ("-", "NEG", "L", None, RVALS | IN_LAMBDA | LIST_EACH),
     ],
    [2,
     ("**", "POW", "R", 1, RVALS | IN_LAMBDA | RANGE_EACH | LIST_EACH),
     ("RT", "ROOT", "R", 1, RVALS | IN_LAMBDA | RANGE_EACH | LIST_EACH),
     ],
    [1,
     ("RT", "SQRT", "L", None, RVALS | IN_LAMBDA | RANGE_EACH | LIST_EACH),
     ],
    [2,
     ("FB", "FROMBASE", "L", 0, RVALS | IN_LAMBDA | RANGE_EACH | LIST_EACH),
     ],
    [2,
     ("@", "AT", "L", None, VALS | IN_LAMBDA),
     ("@<", "LEFTOF", "L", None, VALS | IN_LAMBDA),
     ("@>", "RIGHTOF", "L", None, VALS | IN_LAMBDA),
     ],
    [1,
     ("++", "INC", "L", None, VALS | IN_LAMBDA),
     ("--", "DEC", "L", None, VALS | IN_LAMBDA),
     ("#", "LEN", "L", None, RVALS | IN_LAMBDA),
     ("A", "ASC", "L", None, RVALS | IN_LAMBDA | RANGE_EACH | LIST_EACH),
     ("C", "CHR", "L", None, RVALS | IN_LAMBDA | RANGE_EACH | LIST_EACH),
     ("AB", "ABS", "L", None, RVALS | IN_LAMBDA | RANGE_EACH | LIST_EACH),
     ("SG", "SIGN", "L", None, RVALS | IN_LAMBDA | RANGE_EACH | LIST_EACH),
     ("FB", "FROMBASE", "L", None, RVALS | IN_LAMBDA | RANGE_EACH | LIST_EACH),
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
paren = Operator("PAREN", "PARENTHESIZE", 1, highestPrecedence, "L", None, VALS)
enlist = Operator("LIST", "LIST", 1, highestPrecedence, "L", None, RVALS)
block = Operator("BLOCK", "BLOCK", 1, highestPrecedence, "L", None)
send = Operator("SEND", "SEND", 1, highestPrecedence, "L", None, VALS)
chain = None  # We define this "operator" later to make sure it gets the same
              # precedence as the comparison operators it comprises

for precedence, (arity, *entries) in enumerate(precedenceTable):
    for entry in entries:
        text, function, associativity = entry[:3]
        if len(entry) > 3:
            default = entry[3]
        else:
            default = None
        if len(entry) > 4:
            flags = entry[4]
        else:
            flags = 0
        if arity > 2 and flags & (LIST_EACH | RANGE_EACH):
            # TODO: proper implementation error message
            msg = "Current implementation cannot handle LIST_EACH or RANGE_EACH"
            msg += "\nfor operators of arity greater than 2 (like %s)" % text
            print(msg)
            flags = flags & ~LIST_EACH & ~RANGE_EACH
        if flags & IN_LAMBDA and not flags & (VALS | RVALS):
            # TODO: proper implementation error message
            msg = "IN_LAMBDA may not be set for operators that do not also"
            msg += "\nset VALS or RVALS (%s)" % test
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
