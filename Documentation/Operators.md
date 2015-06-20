**Note**: operators are listed in ASCII/alphabetical order. For up-to-date precedence and associativity information, your best bet is to read the code: there's a reasonably human-readable precedence table in `operators.py`.

U/B/T = Unary/Binary/Ternary

### Meta-operators

`$` Fold: occurs before a binary operator; the resulting compound operator is unary, with the same precedence as the original binary operator

`:` Modify-assign: occurs after a binary *or* unary operator, assigning the result of the calculation back to the lhs

### Symbolic operators

<pre>!  U</pre> Logical not

<pre>!= B</pre> Numeric not equal

<pre>#  U</pre> Length

<pre>#< B</pre> Length less than

<pre>#= B</pre> Length equal

<pre>#> B</pre> Length greater than

<pre>%  B</pre> Modulo

<pre>&  B</pre> Logical and (short-circuiting)

<pre>*  B</pre> Multiplication

<pre>** B</pre> Exponentiation

<pre>+  B</pre> Addition

<pre>+  U</pre> Cast value as number

<pre>++ U</pre> Pre-increment (always comes before operand)

<pre>,  B</pre> Range

<pre>,  U</pre> Range from 0 up to

<pre>-  B</pre> Subtraction

<pre>-  U</pre> Numeric negation

<pre>-- U</pre> Pre-decrement (always comes before operand)

<pre>.  B</pre> Concatenation

<pre>/  B</pre> Division

<pre>// B</pre> Integer division

<pre>:  B</pre> Assignment

<pre><  B</pre> Numeric less than

<pre><= B</pre> Numeric less than or equal

<pre><=>B</pre> Numeric cmp (-1 if less, 0 if equal, 1 if greater)

<pre><> B</pre> Group iterable into sections of given length

<pre><| B</pre> Strip from right

<pre><| U</pre> Strip whitespace from right

<pre>=  B</pre> Numeric equal

<pre>== B</pre> Exactly equal

<pre>>  B</pre> Numeric greater than

<pre>>= B</pre> Numeric greater than or equal

<pre>?  T</pre> If-then-else operator (short-circuiting)

<pre>@  B</pre> Get item/slice at index

<pre>@* B</pre> Find all

<pre>@< B</pre> Slice of string left of index

<pre>@> B</pre> Slice of string right of index

<pre>@? B</pre> Find

<pre>^  B</pre> Split string on separator

<pre>^  U</pre> Split string into list of characters

<pre>^@ B</pre> Split iterable at index or list of indices

<pre>|  B</pre> Logical or (short-circuiting)

<pre>|> B</pre> Strip from left

<pre>|> U</pre> Strip whitespace from left

<pre>|| B</pre> Strip

<pre>|| U</pre> Strip whitespace

### Alphabetic operators

<pre>A  U</pre> Convert (first) char to ASCII value (or Unicode point)

<pre>AB U</pre> Absolute value of number

<pre>AE B</pre> List with appended element

<pre>AL B</pre> List with appended list

<pre>BA B</pre> Bitwise and

<pre>BN U</pre> Bitwise not

<pre>BO B</pre> Bitwise or

<pre>BX B</pre> Bitwise xor

<pre>C  U</pre> Convert ASCII value/Unicode point to character

<pre>CG B</pre> Coordinate grid of i rows by j columns

<pre>CP B</pre> Cartesian product of two iterables

<pre>CP U</pre> Cartesian product of a list of iterables

<pre>EQ B</pre> String equal

<pre>FB B</pre> Convert number from given base to decimal integer

<pre>FB U</pre> Convert number from binary to decimal integer

<pre>FI B</pre> Filter (1st arg is function, 2nd is list)

<pre>GT B</pre> String greater than

<pre>GE B</pre> String greater than or equal

<pre>IN B</pre> In (returns count of occurrences or 0 if none)

<pre>J  B</pre> Join iterable on separator

<pre>J  U</pre> Join iterable on empty string

<pre>LC U</pre> Convert to lowercase

<pre>LT B</pre> String less than

<pre>LE B</pre> String less than or equal

<pre>M  B</pre> Map function to iterable, returning list

<pre>MJ B</pre> Map function to iterable and join results into string

<pre>MN U</pre> Min of iterable using numeric comparison

<pre>MX U</pre> Max of iterable using numeric comparison

<pre>NE B</pre> String not equal

<pre>NI B</pre> Not in (returns truth value 0 or 1)

<pre>PE B</pre> List with prepended element

<pre>R  T</pre> Replace each occurrence in string of substring with replacement

<pre>RC U</pre> Random choice

<pre>RL B</pre> Repeat list

<pre>RM B</pre> From string remove characters; from list remove item

<pre>RP B</pre> Convert to Pip representation

<pre>RR B</pre> Randrange

<pre>RR U</pre> Randrange from 0

<pre>RT B</pre> Nth root

<pre>RT U</pre> Square root

<pre>RV B</pre> Reverse iterable

<pre>SG U</pre> Sign of number (-1, 0, or 1)

<pre>SN U</pre> Sort iterable using numeric comparison

<pre>SS U</pre> Sort iterable using string comparison

<pre>ST B</pre> Convert to string (for lists, the format of the result depends on command-line flags)

<pre>TB B</pre> Convert decimal integer to given base

<pre>TB U</pre> Convert decimal integer to binary

<pre>TM B</pre> Trim string by given number of characters from front and end (takes Scalar for same amt on both sides or Range for different amts)

<pre>TM U</pre> Trim first and last characters from string

<pre>UC U</pre> Convert to UPPERCASE

<pre>UQ U</pre> Keep only unique values from iterable

<pre>V  B</pre> Evaluate function with arglist (fVl is equivalent to f(*l) in Python)

<pre>V  U</pre> Evaluate code block in current context, returning value of final expression or nil if there isn't one

<pre>X  B</pre> Repeat string given number of times

<pre>Z  B</pre> Zip two lists together (clipping to the shorter length)

<pre>Z  U</pre> Zip a list of lists together (clipping to the shortest length)

<pre>ZD B</pre> Zip a list of lists together, filling missing values with given default

<pre>ZD U</pre> Zip a list of lists together, filling missing values with nil

<pre>ZG B</pre> Grid of zeros (i rows by j columns)
