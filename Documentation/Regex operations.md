
Regex operations in Pip use the Pattern data type. Patterns are delimited by <pre>`</pre> (backticks); backticks within the Pattern can be escaped using backslash, as can literal backslashes. Regexes are basically Python flavor with a few add-ons. Any legal Python regex is a legal Pip regex and will do the same thing.

## Differences between Python and Pip

 - Pip Patterns are used both as regexes and as regex replacement strings.
 - In addition to back-references (e.g. `\1`), Pip replacement Patterns can contain `&`, which corresponds to the entire match (as in sed et al.).

## Pip regex operations

Pip currently supports the following regex operations (with more in the works):

### Find all: @

Usage: `s@x`

Returns a List of all non-overlapping matches of Pattern `x` in Scalar `s`.

### Find index: @?

Usage: `s@?x`

Returns the start index of the first match of Pattern `x` in Scalar `s`, or nil if Pattern was not found.

### Find all indices: @*

Usage: `s@*x`

Same as `@?`, but returns a List of all match indices.

### Not in/in/count: NI, IN

Usage: `xINs`

`IN` returns the number of non-overlapping matches of Pattern `x` in Scalar `s`. `NI` returns `1` if the Pattern was not found, `0` if it was.

### Replace: R

Usage: `sRxp`

Replace each match of Pattern `x` in Scalar `s` with replacement (Pattern or Scalar) `p`.
