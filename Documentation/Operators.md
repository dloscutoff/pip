**Note**: operators are listed in ASCII/alphabetical order. For up-to-date precedence and associativity information, your best bet is to read the code: there's a reasonably human-readable precedence table in `operators.py`.

U/B/T = Unary/Binary/Ternary

### Meta-operators

`$` Fold: occurs before a binary operator; the resulting compound operator is unary, with the same precedence as the original binary operator

`:` Modify-assign: occurs after a binary *or* unary operator, assigning the result of the calculation back to the lhs

### Symbolic operators

<code>!&ensp; U</code> Logical not

<code>!= B</code> Numeric not equal

<code>#&ensp; U</code> Length

<code>%&ensp; B</code> Modulo

<code>&&ensp; B</code> Logical and (short-circuiting)

<code>*&ensp; B</code> Multiplication

<code>** B</code> Exponentiation

<code>+&ensp; B</code> Addition

<code>+&ensp; U</code> Cast value as number

<code>++ U</code> Pre-increment (always comes before operand)

<code>,&ensp; B</code> Range

<code>,&ensp; U</code> Range from 0 up to

<code>-&ensp; B</code> Subtraction

<code>-&ensp; U</code> Numeric negation

<code>-- U</code> Pre-decrement (always comes before operand)

<code>.&ensp; B</code> Concatenation

<code>/&ensp; B</code> Division

<code>// B</code> Integer division

<code>:&ensp; B</code> Assignment

<code><&ensp; B</code> Numeric less than

<code><= B</code> Numeric less than or equal

<code><=>B</code> Numeric cmp (-1 if less, 0 if equal, 1 if greater)

<code><> B</code> Group iterable into sections of given length

<code>=&ensp; B</code> Numeric equal

<code>== B</code> Exactly equal

<code>>&ensp; B</code> Numeric greater than

<code>>= B</code> Numeric greater than or equal

<code>?&ensp; T</code> If-then-else operator (short-circuiting)

<code>@&ensp; B</code> Get item/slice at index

<code>@< B</code> Slice of string left of index

<code>@> B</code> Slice of string right of index

<code>^&ensp; B</code> Split string on separator

<code>^&ensp; U</code> Split string into list of characters

<code>|&ensp; B</code> Logical or (short-circuiting)

### Alphabetic operators

<code>A&ensp; U</code> Convert (first) char to ASCII value (or Unicode point)

<code>AE B</code> List with appended element

<code>AL B</code> List with appended list

<code>C&ensp; U</code> Convert ASCII value/Unicode point to character

<code>EQ B</code> String equal

<code>FB B</code> Convert number from given base to decimal integer

<code>FB U</code> Convert number from binary to decimal integer

<code>GT B</code> String greater than

<code>GE B</code> String greater than or equal

<code>IN B</code> In (returns count of occurrences or 0 if none)

<code>J&ensp; B</code> Join iterable on separator

<code>J&ensp; U</code> Join iterable on empty string

<code>LC U</code> Convert to lowercase

<code>LT B</code> String less than

<code>LE B</code> String less than or equal

<code>M&ensp; B</code> Map function to iterable, returning list

<code>MN U</code> Min of iterable using numeric comparison

<code>MX U</code> Max of iterable using numeric comparison

<code>NE B</code> String not equal

<code>NI B</code> Not in (returns truth value 0 or 1)

<code>PE B</code> List with prepended element

<code>R&ensp; T</code> Replace each occurrence in string of substring with replacement

<code>RL B</code> Repeat list

<code>RM B</code> From string remove characters; from list remove item

<code>RP B</code> Convert to Pip representation

<code>RR B</code> Randrange

<code>RR U</code> Randrange from 0

<code>RV B</code> Reverse iterable

<code>SN U</code> Sort iterable using numeric comparison

<code>ST B</code> Convert to string (for lists, the format of the result depends on command-line flags)

<code>TB B</code> Convert decimal integer to given base

<code>TB U</code> Convert decimal integer to binary

<code>UC U</code> Convert to UPPERCASE

<code>UQ U</code> Keep only unique values from iterable

<code>X&ensp; B</code> Repeat string given number of times
