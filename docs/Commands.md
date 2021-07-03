
# List of commands

Each command is given, followed by a pseudocode equivalent and an explanation.

Any command that takes a code block in curly braces `{...}` can drop the braces if the block contains only one statement.

## Loops

### While loop

    Wx<10{...}

    while(x < 10) {...}

Loop as long as the condition is true; stop when it becomes false.

### Till loop

    Tx>9{...}

    while(not x > 9) {...}

Loop as long as the condition is false; stop when it becomes true.

### For loop

    Fil{...}

    foreach(i in l) {...}

Loops the variable `i` over each item in `l`. The variable must be either a single identifier or a (possibly nested) list of identifiers in square brackets. Legal types for the iteration object are Scalar, List, and Range. Expressions that start with a unary operator are fine:

    Fi,20{...}

    foreach(i in range(0, 20)) {...}

### Fixed iterations loop

    L10{...}

    repeat 10 times {...}

Like a for loop, but takes a number rather than an iterable, and does not assign to a loop variable. Equivalent of `{...}*10` in some languages.

### Regex match loop

    LRxs{...}
    
    foreach(regex match of x in s) {...}

Similar to `while $s =~ $x` in Perl. Use [regex special variables](https://github.com/dloscutoff/pip/blob/master/docs/Regex%20operations.md#match-variables) to access match data on each iteration.

## Conditionals

### If statement

    Ix<0{...}EIx>0{...}EL{...}

    if(x < 0) {...} elseif(x < 0) {...} else {...}

Since both the if and else branches may drop the curly braces if they contain single statements, the `EI` is just syntactic sugar for `EL I`.

## Other commands

### Swap statement

    Sab

    swap values of a, b

`a` and `b` can be any two expressions as long as they evaluate to lvalues. For instance, `Sx@0x@1` will swap the first two items in an iterable `x`.

### Wipe globals

    WG

Resets all global variables to their default values.
