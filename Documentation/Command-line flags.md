Pip uses several command-line flags to control input and output options:

### Code source

`-e` Execute the following code

`-f` Execute code in given file

`-i` Execute code read from stdin

When Pip is called without any of these flags, the default is stdin for interactive mode and file for non-interactive.

### List formatting

`-p` When printing lists, Pretty-print them by executing RP on them first

`-s` When printing lists, join them on Space first

`-n` When printing lists, join them on Newline first

`-l` When printing lists, join each item (if a list) on space, then join the list on newline (i.e. print list as multiple Lines)

The default is to join lists on empty string, as in CJam.

### Debugging output

`-v` Verbose mode: currently, prints space-separated list of tokens and parse tree before execution

`-w` Print warning messages for things like div by 0 (instead of silently using nil and moving on)

`-d` Debug mode: equivalent to `-vwp`

### Other

`-r` Instead of getting `g` from command-line args, initialize it from lines of stdin
