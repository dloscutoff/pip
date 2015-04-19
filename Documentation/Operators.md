*Note*: operators are listed in ASCII/alphabetical order. For up-to-date precedence and associativity information, your best bet is to read the code: there's a reasonably human-readable precedence table in `operators.py`.

U/B/T = Unary/Binary/Ternary

**Meta-operators:**
`$` Fold: occurs before a binary operator; the resulting compound operator is unary, with the same precedence as the original binary operator
`:` Modify-assign: occurs after a binary *or* unary operator, assigning the result of the calculation back to the lhs

**Symbolic operators:**
`!  U` Logical not
`!= B` Numeric not equal
`#  U` Length
`%  B` Modulo
`&  B` Logical and (short-circuiting)
`*  B` Multiplication
`** B` Exponentiation
`+  B` Addition
`+  U` Cast value as number
`++ U` Pre-increment (always comes before operand)
`,  B` Range
`,  U` Range from 0 up to
`-  B` Subtraction
`-  U` Numeric negation
`-- U` Pre-decrement (always comes before operand)
`.  B` Concatenation
`/  B` Division
`// B` Integer division
`:  B` Assignment
`<  B` Numeric less than
`<= B` Numeric less than or equal
`<=>B` Numeric cmp (-1 if less, 0 if equal, 1 if greater)
`=  B` Numeric equal
`== B` Exactly equal
`>  B` Numeric greater than
`>= B` Numeric greater than or equal
`?  T` If-then-else operator (short-circuiting)
`@  B` Get item/slice at index
`@< B` Slice of string left of index
`@> B` Slice of string right of index
`^  B` Split string on separator
`^  U` Split string into list of characters
`|  B` Logical or (short-circuiting)

**Alphabetic operators:**
`A  U` Convert (first) char to ASCII value (or Unicode point)
`AE B` List with appended element
`AL B` List with appended list
`C  U` Convert ASCII value/Unicode point to character
`EQ B` String equal
`FB B` Convert number from given base to decimal integer
`FB U` Convert number from binary to decimal integer
`GT B` String greater than
`GE B` String greater than or equal
`IN B` In (returns count of occurrences or 0 if none)
`J  B` Join iterable on separator
`J  U` Join iterable on empty string
`LT B` String less than
`LE B` String less than or equal
`M  B` Map function to iterable, returning list
`NE B` String not equal
`NI B` Not in (returns truth value 0 or 1)
`PE B` List with prepended element
`R  T` Replace each occurrence in string of substring with replacement
`RM B` From string remove characters; from list remove item
`RP B` Convert to Pip representation
`RR B` Randrange
`RR U` Randrange from 0
`RV B` Reverse iterable
`ST B` Convert to string (for lists, the format of the result depends on command-line flags)
`TB B` Convert decimal integer to given base
`TB U` Convert decimal integer to binary
`X  B` Repeat string given number of times
