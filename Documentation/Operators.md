**Note**: operators are listed in ASCII/alphabetical order. For up-to-date precedence and associativity information, your best bet is to read the code: there's a reasonably human-readable precedence table in `operators.py`.

U/B/T = Unary/Binary/Ternary

### Meta-operators

`$` Fold: occurs before a binary operator; the resulting compound operator is unary, with the same precedence as the original binary operator

`*` Map: occurs after a unary operator; arity and precedence remain the same

`:` Modify-assign: occurs after a unary, binary, or ternary operator; the resulting compound operator has the same arity but has the precedence of `:`

### Symbolic operators

<code>!  U</code> Logical not

<code>!= B</code> Numeric not equal

<code>#  U</code> Length

<code>#< B</code> Length less than

<code>#= B</code> Length equal

<code>#> B</code> Length greater than

<code>%  B</code> Modulo

<code>&  B</code> Logical and (short-circuiting)

<code>*  B</code> Multiplication

<code>** B</code> Exponentiation

<code>+  B</code> Addition

<code>+  U</code> Cast value as number; set dotall flag on Pattern

<code>++ U</code> Pre-increment (always comes before operand)

<code>,  B</code> Range

<code>,  U</code> Range from 0 up to; set multiline flag on Pattern

<code>-  B</code> Subtraction

<code>-  U</code> Numeric negation; set case-insensitive flag on Pattern

<code>-- U</code> Pre-decrement (always comes before operand)

<code>.  B</code> Concatenation

<code>/  B</code> Division

<code>/  B</code> Inverse

<code>// B</code> Integer division

<code>:  B</code> Assignment

<code><  B</code> Numeric less than

<code><= B</code> Numeric less than or equal

<code><=>B</code> Numeric cmp (-1 if less, 0 if equal, 1 if greater)

<code><> B</code> Group iterable into sections of given length

<code><| B</code> Strip from right

<code><| U</code> Strip whitespace from right

<code>=  B</code> Numeric equal

<code>== B</code> Exactly equal

<code>>  B</code> Numeric greater than

<code>>= B</code> Numeric greater than or equal

<code>?  T</code> If-then-else operator (short-circuiting)

<code>@  B</code> Get item/slice at index

<code>@* B</code> Find all

<code>@< B</code> Slice of iterable left of index

<code>@< U</code> Leftmost item of iterable

<code>@> B</code> Slice of iterable right of index

<code>@> B</code> Rightmost item of iterable

<code>@? B</code> Find

<code>^  B</code> Split Scalar on separator

<code>^  U</code> Split Scalar into List of characters

<code>^@ B</code> Split iterable at index or List of indices

<code>|  B</code> Logical or (short-circuiting)

<code>|> B</code> Strip from left

<code>|> U</code> Strip whitespace from left

<code>|| B</code> Strip

<code>|| U</code> Strip whitespace

### Alphabetic operators

<code>A  U</code> Convert (first) char to ASCII value (or Unicode point); set ASCII-only flag on Pattern

<code>AB U</code> Absolute value of number

<code>AE B</code> List with appended element

<code>AL B</code> List with appended List

<code>AT B</code> Arctangent (operands are y and x)

<code>AT U</code> Arctangent

<code>BA B</code> Bitwise and

<code>BN U</code> Bitwise not

<code>BO B</code> Bitwise or

<code>BX B</code> Bitwise xor

<code>C  U</code> Convert ASCII value/Unicode point to character

<code>CB B</code> List of all combinations from iterable of n elements

<code>CG B</code> Coordinate grid of i rows by j columns

<code>CG U</code> Coordinate grid of i rows by i columns

<code>CO U</code> Cosine

<code>CP B</code> Cartesian product of two iterables

<code>CP U</code> Cartesian product of a List of iterables

<code>CS U</code> Cosecant

<code>CT U</code> Cotangent

<code>DG U</code> Convert radians to degrees

<code>DQ U</code> Dequeue item from back of iterable (modifying argument in-place)

<code>EN U</code> Enumerate an iterable (gives List of `[index item]` Lists)

<code>EQ B</code> Alias for binary Q, kept for backwards compatibility

<code>FB B</code> Convert number from given base to decimal integer

<code>FB U</code> Convert number from binary to decimal integer

<code>FI B</code> Filter (1st arg is Block, 2nd is List)

<code>GT B</code> String greater than

<code>GE B</code> String greater than or equal

<code>IN B</code> Alias for binary N, kept for backwards compatibility

<code>J  B</code> Join iterable on separator

<code>J  U</code> Join iterable on empty string

<code>JW B</code> Join iterable on separator and wrap result in separator as well

<code>K  U</code> Applies Kleene star (repeat 0 or more times) to a Pattern

<code>LC U</code> Convert to lowercase

<code>LT B</code> String less than

<code>LE B</code> String less than or equal

<code>M  B</code> Map Block to iterable, returning List

<code>MJ B</code> Map Block to iterable and join results into Scalar

<code>MM B</code> Map Block to each subitem of iterable, returning List of Lists

<code>MN U</code> Min of iterable using numeric comparison

<code>MS B</code> Map Block to iterable and sum results

<code>MX U</code> Max of iterable using numeric comparison

<code>MZ T</code> Map Block to two iterables, passing corresponding pairs of elements as arguments; returns List

<code>N  B</code> In (returns count of occurrences or 0 if none)

<code>NE B</code> String not equal

<code>NI B</code> Not in (returns truth value 0 or 1)

<code>O  U</code> Output value and pass through unchanged (same as P but without trailing newline)

<code>P  U</code> Print value with newline and pass through unchanged (output format for Lists depends on command-line flags; Nil gives no output, including trailing newline)

<code>PB B</code> Push item to back of iterable (modifying argument in-place)

<code>PE B</code> List with prepended element

<code>PM U</code> List of all permutations of iterable

<code>PO U</code> Pop item from front of iterable (modifying argument in-place)

<code>PU B</code> Push item to front of iterable (modifying argument in-place)

<code>Q  B</code> String equal

<code>R  T</code> Replace each occurrence in Scalar of substring or Pattern with replacement

<code>RC U</code> Random choice

<code>RD U</code> Convert degrees to radians

<code>RL B</code> Repeat List

<code>RM B</code> From Scalar remove characters; from List remove item

<code>RP B</code> Convert to Pip representation

<code>RR B</code> Randrange

<code>RR U</code> Randrange from 0

<code>RT B</code> Nth root

<code>RT U</code> Square root

<code>RV B</code> Reverse iterable

<code>SE U</code> Secant

<code>SG U</code> Sign of number (-1, 0, or 1)

<code>SI U</code> Sine

<code>SK B</code> Sort iterable using Block as key function

<code>SN U</code> Sort iterable using numeric comparison

<code>SS U</code> Sort iterable using string comparison

<code>ST B</code> Convert to string (for Lists, the format of the result depends on command-line flags)

<code>TA U</code> Tangent

<code>TB B</code> Convert decimal integer to given base

<code>TB U</code> Convert decimal integer to binary

<code>TM B</code> Trim Scalar by given number of characters from front and end (Scalar rhs trims same amt on both sides; Range rhs trims different amts)

<code>TM U</code> Trim first and last characters from Scalar

<code>UC U</code> Convert to UPPERCASE

<code>UQ U</code> Keep only unique values from iterable

<code>V  B</code> Call Block as function with arglist (fVl is equivalent to f(*l) in Python)

<code>V  U</code> Evaluate Block in current context, returning value of final expression or Nil if there isn't one

<code>WR B</code> Wrap Scalar or Pattern with a delimiter (or List of two delimiters)

<code>X  B</code> Repeat Scalar given number of times

<code>X  U</code> Covert Scalar or List/Range to equivalent regex Pattern (escaping special characters as necessary)

<code>Y  U</code> Yank value into the variable `y` (`Yx` is equivalent to `(y:x)`)

<code>Z  B</code> Zip two Lists together (clipping to the shorter length)

<code>Z  U</code> Zip a List of Lists together (clipping to the shortest length)

<code>ZD B</code> Zip a List of Lists together, filling missing values with given default

<code>ZD U</code> Zip a List of Lists together, filling missing values with Nil

<code>ZG B</code> Grid of zeros (i rows by j columns)

<code>ZG U</code> Grid of zeros (i rows by i columns)
