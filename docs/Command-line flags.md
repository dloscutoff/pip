
# Command-line flags

Pip uses several command-line flags to control input and output options.

## Code source

`-e` Execute the following code

`-f` Execute code in given file

`-i` Execute code read from stdin

When Pip is called without any of these flags, the default is stdin for interactive mode and file for non-interactive.

## Input interpretation

`-x` Instead of setting `g` to the list of command-line args as Scalars, evaluate each arg as Pip code and set `g` to the list of resulting values

## Input source

`-r` Instead of setting `g` to the list of command-line args, set it to the list of all lines read from stdin

## List formatting

If no flag is specified, the default is to join lists on empty string.

`-p` Pretty-print lists by executing `RP` on them first

`-s` Join lists on space

`-n` Join lists on newline

`-l` Join each item on empty string, then join results on newline (i.e. print list as multiple lines)

`-P` Execute `RP` on each item, then join results on newline

`-S` Join each item on space, then join results on newline

## Debugging output

`-v` Verbose mode: print space-separated list of tokens and parse tree before execution

`-w` Print warning messages for things like division by 0 (instead of silently using nil and moving on)

`-d` Debug mode: equivalent to `-vwp`
