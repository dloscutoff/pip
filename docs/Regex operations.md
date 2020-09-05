
# Regex operations

Regex operations in Pip use the Pattern data type.

Patterns are delimited by ``` ` ``` (backticks).

Backticks within the Pattern can be escaped using backslash, as can literal backslashes. Regexes are basically Python flavor with a few add-ons. Any legal Python regex is a legal Pip regex (as long as backticks and `&` are escaped) and will behave the same way.

## Differences between Python and Pip

 - Pip Patterns are used both as regexes and as regex replacement strings.
 - In addition to back-references (e.g. `\1`), Pip replacement Patterns can contain `&`, which corresponds to the entire match (as in sed et al.).
 - Many Pip regex operations set special variables similar to the ones in Perl, rather than the Python strategy of returning a match object encapsulating that information.

## Predefined Pattern variables

Some common regexes are available as predefined [variables](https://github.com/dloscutoff/pip/blob/master/docs/Variables.md):

Variable | Value                     | Mnemonic
-------- | ------------------------- | ----------------
`w`      | `\s+`                     | Whitespace
`XA`     | `[A-Za-z]`                | regeX Alpha
`XC`     | `[bcdfghjklmnpqrstvwxyz]` | regeX Consonant
`XD`     | `\d`                      | regeX Digit
`XI`     | `-?\d+`                   | regeX Integer
`XL`     | `[a-z]`                   | regeX Lowercase
`XN`     | `-?\d+(?:\.\d+)?`         | regeX Number
`XU`     | `[A-Z]`                   | regeX Uppercase
`XV`     | `[aeiou]`                 | regeX Vowel
`XW`     | `\w`                      | regeX Word
`XX`     | `.`                       | regeX anything
`XY`     | `[aeiouy]`                | regeX vowel-or-Y

## Regex-building operations

The following operators can be used to build regexes:

### Concatenate/repeat (low level): `.` `X`

Usage: `x.s` `xXn`

Both binary operators work the same as with Scalars. They consider only the text of the Pattern, whether it is a regex, a fragment of a regex, or a replacement. Concatenating a Scalar and a Pattern coerces the result to Pattern.

### Concatenate/alternate/repeat (high level): `+` `,` `*`

Usage: `x+y` `x,y` `x*n`

`+` and `,` assume both operands are valid regexes, wrap each in a non-capturing group, and concatenate them, with `,` placing a `|` in between. `*` assumes the first operand is a valid regex, wraps it in a non-capturing group, and appends a repetition construct like `{n}`.

### Convert to regex: `X`

Usage: `Xs`

Converts a Scalar to a Pattern, escaping special characters. Given a List or Range, converts to a Pattern that will match any of the items.

### Repetition/grouping: `K` `+` `C`

Usage: `Kx`

`K` and `+` modify a Pattern with `*` or `+`, respectively. `C` wraps a pattern in a capturing group.

NOTE: `K` also works on Scalars and Ranges, converting them to Patterns first. `+` and `C` only work on Patterns.

### Set flags: `-` `.` `,` `A`

Usage: `-x`

The unary operators `-` `.` `,` `A` set the case-insensitive, dotall, multiline, and ASCII-only regex flags, respectively. See the Python 3 `re` docs for more information.
 
## Pip regex operations

Pip currently supports the following regex operations (with more in the works):

### First match: `~`

Usage: `s~x`

Returns the first match of Pattern `x` in Scalar `s`, or nil if no match was found. Can also be used as `x~s`.

### All matches: `@`

Usage: `s@x`

Returns a List of all non-overlapping matches of Pattern `x` in Scalar `s`.

### Find index: `@?`

Usage: `s@?x`

Returns the start index of the first match of Pattern `x` in Scalar `s`, or nil if no match was found.

### Find all indices: `@*`

Usage: `s@*x`

Same as `@?`, but returns a List of all match indices.

### Not in/in/count: `NI`, `N`

Usage: `xNs`

`N` returns the number of non-overlapping matches of Pattern `x` in Scalar `s`. `NI` returns `1` if the Pattern was not found, `0` if it was.

### Fullmatch: `~=`

Usage: `x~=s`

Returns `1` if Pattern `x` fully matches Scalar `s`, `0` otherwise. Can be chained with other comparison operators. Can also be used as `s~=x`.

### Replace: `R`

Usage: `sRxp`

Replace each non-overlapping match of Pattern `x` in Scalar `s` with replacement (Pattern, Scalar or callback function) `p`. The arguments passed to a callback function are the entire match (parameter `a`) followed by capture groups (parameters `b` through `e`).

### Remove: `RM`

Usage: `sRMx`

Remove each non-overlapping match of Pattern `x` from Scalar `s`.

### Strip/lstrip/rstrip: `||` `|>` `<|`

Usage: `s||x`

Strip matches of Pattern `x` from the left, right, or both sides of Scalar `s`.

### Split: `^`

Usage: `s^x`

Split Scalar `s` on occurrences of Pattern `x`. If `x` contains capture groups, they are included in the resulting List.

### Map: `MR`

Usage: `fMRxs`

Find all matches of Pattern `x` in Scalar `s` and map function `f` to them. The arguments passed to the function are the entire match (parameter `a`) followed by capture groups (parameters `b` through `e`). Can also be used as `sMRxf` or `fMRsx`.

### Loop: `LR`

Usage: `LRxs{...}`

The command version of `MR`: loops over all matches of Pattern `x` in Scalar `s`. Use regex special variables to access match information inside the loop. Can also be used as `LRsx{...}`.

## Match variables

The following regex match variables are set every time a match is made by most regex operations--most usefully, `MR`, `LR`, and `R`:

- `$0`: entire match
- `$1`: capture group 1 (and similarly for 2-9)
- `$$`: list of all capture group contents
- `$(`: start index of match
- `$)`: end index of match
- `$[`: list of start indices of capture groups
- `$]`: list of end indices of capture groups
- <code>$`</code>: the part of the string before the match
- `$'`: the part of the string after the match
