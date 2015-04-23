Each command is given, followed by pseudocode equivalent and an explanation.

### For loop

`Fil{...}`

`foreach(i in l) {...}`

Loops the variable `i` over each item in `l`. Legal types for the iteration object are scalar, list, and range. The variable must be a single identifier; this requirement allows the following very useful syntax with the unary range operator:

`Fi,20{...}`

`foreach(i in range(0, 20)) {...}`

### If statement

`Ix<0{...}EIx>0{...}E{...}`

`if(x < 0) {...} elseif(x < 0) {...} else {...}`

Since both the if and else branches may drop the curly braces if they contain single statements, the `EI` is just syntactic sugar for `E I`.

### Fixed iterations loop

`L10{...}`

`repeat 10 times {...}`

Like a for loop, but takes a number rather than an iterable, and does not assign to a loop variable. Equivalent of `{...}*10` in some languages.

### Output statement

`O"Hello world"`

`write("Hello world")`

Same as `P`, but does not output a newline at the end.

### Print statement

`P"Hello world"`

`writeline("Hello world")`

Converts value to a string using `ST` and outputs the result. Always has a trailing newline, unless the value is nil, in which case no output happens at all. **Note**: only takes a single expression! To print more than one value on the same line, use string concatenation or put them in a list.

The last bare expression in a program is automatically printed. To suppress printing, end the program with statement or a nil expression.

The format in which lists are printed depends on command-line flags; see that documentation page for details.

### Query statement

`Qx`

`x := readline()`

Gets a line of input from stdin, and stores it (sans newline) into the specified variable.

### Swap statement

`Sab`

`swap values of a, b`

### Till loop

`Tx>9{...}`

`while(not x > 9) {...}`

Loop as long as the condition is false; stop when it becomes true.

### While loop

`Wx<10{...}`

`while(x < 10) {...}`

Loop as long as the condition is true; stop when it becomes false.
