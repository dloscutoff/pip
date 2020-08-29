---
layout: default
title: Command Line Flags
permalink: cli-flags
---

# Command Line Flags

Pip uses several command-line flags to control input and output options:

## Code source

`-e` Execute the following code

`-f` Execute code in given file

`-i` Execute code read from stdin

When Pip is called without any of these flags, the default is stdin for interactive mode and file for non-interactive.

## List formatting

`-p` Pretty-print lists by executing RP on them first

`-s` Join lists on space

`-n` Join lists on newline

`-l` Join each item on empty string, then join results on newline (i.e. print list as multiple lines)

`-P` Execute RP on each item, then join results on newline

`-S` Join each item on space, then join results on newline

The default is to join lists on empty string, as in CJam.

## Debugging output

`-v` Verbose mode: currently, prints space-separated list of tokens and parse tree before execution

`-w` Print warning messages for things like div by 0 (instead of silently using nil and moving on)

`-d` Debug mode: equivalent to `-vwp`

## Other

`-r` Instead of getting `g` from command-line args, initialize it from lines of stdin
