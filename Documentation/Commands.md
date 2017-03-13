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

### Swap statement

`Sab`

`swap values of a, b`

### Till loop

`Tx>9{...}`

`while(not x > 9) {...}`

Loop as long as the condition is false; stop when it becomes true.

### Unify

`UxyzWg`

`x,y,z = g`

Read as "unify x, y, z with g." Like Python's tuple unpacking, but works even if the size of the list doesn't match the number of variables: if the list is too long, the remainder is ignored; if the list is too short, the latter variables get values of nil.

### While loop

`Wx<10{...}`

`while(x < 10) {...}`

Loop as long as the condition is true; stop when it becomes false.

### Wipe globals

`WG`

Resets all global variables to their default values.