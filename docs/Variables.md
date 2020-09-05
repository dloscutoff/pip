This list also includes the "special variables," which have different behavior when accessed and/or assigned. (For regex match variables, see [Regex operations](https://github.com/dloscutoff/pip/blob/master/Documentation/Regex%20operations.md).) Eventually, every letter h-z should appear below.

`_` Identity function (== `{a}`)

`h` 100

`i` 0

`k` `", "`

`l` Empty list

`m` 1000 (mnemonic: Roman numeral M)

`n` Newline character

`o` 1

`q` Special variable: reads and returns a line of input each time it is accessed

`r` Special variable: returns a random number 0 <= `r` < 1 when it is accessed; assigning to `r` seeds the random-number generator

`s` Space character

`t` 10

`u` Nil

`v` -1

`w` Pattern matching one or more whitespace characters (<code>\`\s+\`</code>)

`x` Empty string

`y` Empty string (modified by `Y` operator)

`z` Lowercase alphabet a to z

`B` Block that returns its second argument (`{b}`)

`G` Block that returns its argument list (`{g}`)

`AZ` Uppercase alphabet A to Z

`CZ` Lowercase consonants b to z

`PA` All **P**rintable **A**SCII characters, 32 through 126

`PI` Pi (3.141592653589793)

`VW` Lowercase vowels a to u

`VY` Lowercase vowels a to y

`XA` Pattern matching one (ASCII) letter (<code>\`[A-Za-z]\`</code>)

`XC` Pattern matching one (lowercase ASCII) consonant (<code>\`[bcdfghjklmnpqrstvwxyz]\`</code>)

`XD` Pattern matching one digit (<code>\`\d\`</code>)

`XI` Pattern matching an integer (<code>\`-?\d+\`</code>)

`XL` Pattern matching one lowercase (ASCII) letter (<code>\`[a-z]\`</code>)

`XN` Pattern matching an integer or decimal number (<code>\`-?\d+(?:\.\d+)?\`</code>)

`XU` Pattern matching one uppercase (ASCII) letter (<code>\`[A-Z]\`</code>)

`XV` Pattern matching one (lowercase ASCII) vowel, not including y (<code>\`[aeiou]\`</code>)

`XW` Pattern matching one word character--letter, number, or underscore (<code>\`\w\`</code>)

`XX` Pattern matching any one character (<code>\`.\`</code>)

`XY` Pattern matching one (lowercase ASCII) vowel, including y (<code>\`[aeiouy]\`</code>)
