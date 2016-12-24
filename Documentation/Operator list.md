Operators are listed in ASCII order. See also the [precedence table](https://github.com/dloscutoff/pip/blob/master/Documentation/Precedence%20table.md). For guaranteed up-to-date information, your best bet is to read the code: there's a reasonably human-readable precedence table in `operators.py`.

### Meta-operators

`$` Fold: occurs before a binary operator; the resulting compound operator is unary, with the same precedence as the original binary operator

`*` Map: occurs after a unary operator; arity and precedence remain the same

`:` Modify-assign: occurs after a unary, binary, or ternary operator; the resulting compound operator has the same arity but has the precedence of `:`

### Operators

<code>!a</code> Logical not

<code>a!=b</code> Numeric not equal

<code>#a</code> Length

<code>a#<b</code> Length less than

<code>a#=b</code> Length equal

<code>a#>b</code> Length greater than

<code>a%b</code> Modulo

<code>a&b</code> Logical and (short-circuiting)

<code>a*b</code> Multiplication; regex repetition

<code>a**b</code> Exponentiation

<code>a+b</code> Addition; regex concatenation

<code>+a</code> Cast value as number

<code>++a</code> Pre-increment (always comes before operand, never `a++`)

<code>a,b</code> Range; regex alternation

<code>,a</code> Range from 0 up to; set multiline flag on Pattern

<code>a-b</code> Subtraction

<code>-a</code> Numeric negation; set case-insensitive flag on Pattern

<code>--a</code> Pre-decrement (always comes before operand, never `a--`)

<code>a.b</code> Concatenation

<code>.a</code> Set dotall flag on Pattern; no-op on other data types

<code>a/b</code> Division

<code>/a</code> Inverse

<code>a//b</code> Integer division

<code>a:b</code> Assignment

<code>a<b</code> Numeric less than

<code>a<=b</code> Numeric less than or equal

<code>a<>b</code> Group iterable `a` into sections of length `b`

<code>a<|b</code> Strip characters in `b` from right of `a`

<code><|a</code> Strip whitespace from right of `a`

<code>a=b</code> Numeric equal

<code>a==b</code> Exactly equal

<code>a>b</code> Numeric greater than

<code>a>=b</code> Numeric greater than or equal

<code>a?bc</code> If-then-else operator (short-circuiting)

<code>a@b</code> Get item/slice at index

<code>a@*b</code> Find all indices of item in iterable, substring or regex match in Scalar

<code>a@<b</code> Slice of iterable left of index

<code>@<a</code> Leftmost item of iterable

<code>a@>b</code> Slice of iterable right of index

<code>@>a</code> Rightmost item of iterable

<code>a@?b</code> Find first index of item in iterable, substring or regex match in Scalar

<code>Aa</code> Convert first char of Scalar to ASCII value (or Unicode point); set ASCII-only flag on Pattern

<code>ABa</code> Absolute value of number

<code>aAEb</code> List `a` with element `b` appended

<code>aALb</code> List `a` with List `b` appended

<code>aATb</code> Arctan2 (with `a` being the y-coordinate and `b` being the x-coordinate)

<code>ATa</code> Arctangent

<code>aBAb</code> Bitwise and

<code>aBNb</code> Bitwise not

<code>aBOb</code> Bitwise or

<code>aBXb</code> Bitwise xor

<code>Ca</code> Convert ASCII value/Unicode point to character

<code>aCBb</code> List of all combinations of `b` elements from iterable `a`

<code>aCGb</code> Coordinate grid of `a` rows by `b` columns

<code>CGa</code> Coordinate grid of `a` rows by `a` columns

<code>aCMb</code> Numeric cmp (-1 if less, 0 if equal, 1 if greater)

<code>COa</code> Cosine

<code>aCPb</code> Cartesian product of two iterables

<code>CPa</code> Cartesian product of a List of iterables

<code>CSa</code> Cosecant

<code>CTa</code> Cotangent

<code>DGa</code> Convert radians to degrees

<code>DQa</code> Dequeue item from back of iterable (modifying argument in-place)

<code>ENa</code> Enumerate an iterable (gives List of `[index item]` Lists)

<code>aEQb</code> Alias for binary `Q`, kept for backwards compatibility

<code>EYa</code> Identity matrix (abbreviation from Matlab's eye() function)

<code>aFBb</code> Convert number from given base to decimal integer

<code>FBa/code> Convert number from binary to decimal integer

<code>aFIb</code> Filter (1st arg is Block, 2nd is List)

<code>aGTb</code> String greater than

<code>aGEb</code> String greater than or equal

<code>aINb</code> Alias for binary `N`, kept for backwards compatibility

<code>aJb</code> Join iterable on separator

<code>Ja</code> Join iterable on empty string

<code>aJWb</code> Join iterable on separator and wrap result in separator as well

<code>Ka</code> Applies Kleene star (repeat 0 or more times) to a Pattern

<code>LCa</code> Convert to lowercase

<code>aLTb</code> String less than

<code>aLEb</code> String less than or equal

<code>aMb</code> Map Block to iterable, returning List

<code>aMCb</code> Map Block `a` to each x,y in `b`x`b` grid of coordinate pairs

<code>aMJb</code> Map Block to iterable and join results into Scalar

<code>aMMb</code> Map Block to each subitem of iterable, returning List of Lists

<code>MNa</code> Min of iterable using numeric comparison

<code>aMPb</code> Map Block to consecutive pairs of items from iterable, returning List

<code>aMSb</code> Map Block to iterable and sum results

<code>aMUb</code> Map Block to iterable, unpacking each item as function arguments (like Python's `itertools.starmap`); returns List

<code>MXa</code> Max of iterable using numeric comparison

<code>aMZbc</code> Map Block to two iterables, passing zipped pairs of elements as arguments; returns List

<code>aNb</code> In (returns count of occurrences or 0 if none)

<code>aNEb</code> String not equal

<code>aNIb</code> Not in (returns truth value 0 or 1)

<code>Oa</code> Output value and pass through unchanged (same as `P` but without trailing newline)

<code>Pa</code> Print value with newline and pass through unchanged (output format for Lists depends on command-line flags; nil gives no output, including trailing newline)

<code>aPBb</code> Push item to back of iterable (modifying argument in-place)

<code>aPEb</code> List `a` with element `b` prepended

<code>PMa</code> List of all permutations of iterable

<code>POa</code> Pop item from front of iterable (modifying argument in-place)

<code>aPUb</code> Push item to front of iterable (modifying argument in-place)

<code>aQb</code> String equal

<code>aRbc</code> Replace each occurrence in Scalar `a` of substring or Pattern `b` with replacement `c`

<code>aRAbc</code> Replace item in iterable `a` at index `b` with replacement `c`

<code>RCa</code> Uniformly random choice of single item from iterable

<code>RDa</code> Convert degrees to radians

<code>aRLb</code> Repeat List `a` `b` times

<code>aRMb</code> From Scalar remove characters; from List remove item

<code>RPa</code> Convert to Pip representation

<code>aRRb</code> Randrange from `a` to `b`

<code>RRa</code> Randrange from 0 to `a`

<code>aRTb</code> `a`th root of `b`

<code>RTa</code> Square root of `a`

<code>RVa</code> Reverse iterable

<code>SEa</code> Secant

<code>SGa</code> Sign of number (-1, 0, or 1)

<code>SIa</code> Sine

<code>aSKb</code> Sort iterable using Block as key function

<code>SNa</code> Sort iterable using numeric comparison

<code>SSa</code> Sort iterable using string comparison

<code>STa</code> Convert to string (for Lists, the format of the result depends on command-line flags)

<code>TAa</code> Tangent

<code>aTBb</code> Convert decimal integer `a` to base `b`

<code>TBa</code> Convert decimal integer `a` to binary

<code>aTMb</code> Trim Scalar `a` by `b` characters from front and end (Scalar `b` trims same amt on both sides; Range `b` trims different amts)

<code>TMa</code> Trim first and last characters from Scalar `a`

<code>UCa</code> Convert to UPPERCASE

<code>UQa</code> Keep only unique values from iterable

<code>aVb</code> Call Block as function with arglist (equivalent to `a(*b)` in Python)

<code>Va</code> Evaluate Block in current context, returning value of final expression or nil if there isn't one

<code>aWRb</code> Wrap Scalar or Pattern with a delimiter (or List of two delimiters)

<code>aWVb</code> Weave two iterables together, alternating their items

<code>WVa</code> Weave all subitems in an iterable together

<code>aXb</code> Repeat Scalar `a` `b` times

<code>Xa</code> Covert Scalar or List/Range to equivalent regex Pattern (escaping special characters as necessary)

<code>Ya</code> Yank value into the variable `y` (equivalent to `(y:a)`)

<code>aZb</code> Zip two Lists together (clipping to the shorter length)

<code>Za</code> Zip a List of Lists together (clipping to the shortest length)

<code>aZDb</code> Zip a List of Lists together, filling missing values with default value `b`

<code>ZDa</code> Zip a List of Lists together, filling missing values with nil

<code>aZGb</code> Grid of zeros (`a` rows by `b` columns)

<code>ZGa</code> Grid of zeros (`a` rows by `a` columns)

<code>a^b</code> Split Scalar `a` on separator `b`

<code>^a</code> Split Scalar `a` into List of characters

<code>a^@b</code> Split iterable `a` at index or List of indices `b`

<code>a|b</code> Logical or (short-circuiting)

<code>a|>b</code> Strip characters in `b` from left of `a`

<code>|>a</code> Strip whitespace from left of `a`

<code>a||b</code> Strip characters in `b` from `a`

<code>||a</code> Strip whitespace from `a`
