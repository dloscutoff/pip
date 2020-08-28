---
title: Variables
layout: default
permalink: vars
---
# Variables

There are four kinds of variables in Pip:

- *Local variables* have different bindings at each scope level. Only their values at the current scope are accessible.
- *Global variables* have the same bindings anywhere in a Pip program. Many of them have useful initial values.
- *Special variables* have side effects when accessed and/or assigned.
- *Regex match variables* can be accessed normally, but they are automatically set each time a regex match is completed. See [Regex operations](https://github.com/dloscutoff/pip/blob/master/Documentation/Regex%20operations.md).

## Local variables

`a` First argument of the current function (at top level, first command-line argument)

`b` Second argument of the current function (at top level, second command-line argument)

`c` Third argument of the current function (at top level, third command-line argument)

`d` Fourth argument of the current function (at top level, fourth command-line argument)

`e` Fifth argument of the current function (at top level, fifth command-line argument)

`f` The current function (at top level, the full program as a function)

`g` Full list of arguments of the current function (at top level, full list of command-line arguments)

## Special variables

`q` Reads and returns a line of input each time it is accessed

`r` Returns a random number 0 <= `r` < 1 when it is accessed; assigning to `r` seeds the random-number generator

## Global variables

Note: any sequence of two uppercase letters that isn't a command or an operator is a global variable. The ones not listed here are initialized to nil.

`_` Identity function (== `{a}`)

`h` 100

`i` 0

`j` TBD; currently initialized to nil

`k` `", "`

`l` Empty list

`m` 1000 (mnemonic: Roman numeral M)

`n` Newline character

`o` 1

`p` TBD; currently initialized to nil

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
