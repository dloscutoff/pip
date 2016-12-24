
Regex operations in Pip use the Pattern data type. Patterns are delimited by <code>`</code> (backticks); backticks within the Pattern can be escaped using backslash, as can literal backslashes. Regexes are basically Python flavor with a few add-ons. Any legal Python regex is a legal Pip regex (as long as backticks are escaped) and will behave the same way.

## Differences between Python and Pip

 - Pip Patterns are used both as regexes and as regex replacement strings.
 - In addition to back-references (e.g. `\1`), Pip replacement Patterns can contain `&`, which corresponds to the entire match (as in sed et al.).

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

### Find all: `@`

Usage: `s@x`

Returns a List of all non-overlapping matches of Pattern `x` in Scalar `s`.

### Find index: `@?`

Usage: `s@?x`

Returns the start index of the first match of Pattern `x` in Scalar `s`, or nil if Pattern was not found.

### Find all indices: `@*`

Usage: `s@*x`

Same as `@?`, but returns a List of all match indices.

### Not in/in/count: `NI`, `N`

Usage: `xNs`

`N` returns the number of non-overlapping matches of Pattern `x` in Scalar `s`. `NI` returns `1` if the Pattern was not found, `0` if it was.

### Replace: `R`

Usage: `sRxp`

Replace each match of Pattern `x` in Scalar `s` with replacement (Pattern, Scalar or callback function) `p`. The arguments passed to a callback function are the entire match (parameter `a`) followed by capture groups (parameters `b` through `e`).

### Split: `^`

Usage: `s^x`

Split Scalar `s` on occurrences of Pattern `x`. If `x` contains capture groups, they are included in the resulting List.
