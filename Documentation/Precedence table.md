
For descriptions of each operator, see the [operator list](https://github.com/dloscutoff/pip/blob/master/Documentation/Operator%20list.md). For guaranteed up-to-date information, your best bet is to read the code: there's a reasonably human-readable precedence table in `operators.py`.

**Chaining** associativity means multiple comparisons can be strung together, like `x<y=z`. **Fold default** is the result of folding an empty iterable on this operator. **Itemwise** indicates whether the operator applies item-by-item to lists, both lists and ranges, or neither. **In lambda** indicates whether the operator can be applied to functions (typically starting from the identity function `_`) to build bigger functions.

Category | Arity | Associativity | Symbol | Name | Fold default | Itemwise? | In lambda?
-------- | ----- | ------------- | ------ | ---- | ------------ | --------- | ----------
Output and yank operators | Unary | – | `O` <br> `P` <br> `Y` | Output <br> Print <br> Yank | None <br> None <br> None | No <br> No <br> No | No <br> No <br> No
Assignment | Binary | Right | `:` | Assign | None | No | No
If-then-else | Ternary | Right | `?` | Ifte | None | No | No
Logical or | Binary | Left | `|` | Or | `0` | No | No
Logical and | Binary | Left | `&` | And | `1` | No | No
Logical not | Unary | – | `!` | Not | None | No | No
Exact equality | Binary | Left | `==` | Objequal | `1` | No | No
Map and other function operators | Binary | Right | `M` <br> `MM` <br> `MJ` <br> `MS` <br> `MU` <br> `MP` <br> `MC` <br> `FI` <br> `SK` <br> `V` | Map <br> Mapmap <br> Mapjoin <br> Mapsum <br> Mapunpack <br> Mappairs <br> Mapcoords <br> Filter <br> Sortkeyed <br> Eval | `[]` <br> `[]` <br> `""` <br> `0` <br> `[]` <br> `[]` <br> `[]` <br> `[]` <br> `[]` <br> None | No <br> No <br> No <br> No <br> No <br> No <br> No <br> No <br> No <br> No | No <br> No <br> No <br> No <br> No <br> No <br> No <br> No <br> No <br> No
Ternary map | Ternary | Right | `MZ` | Mapzip | `[]` | No | No
Eval | Unary | – | `V` | Eval | None | No | No
Comparison operators | Binary | Chaining | `<` <br> `>` <br> `=` <br> `<=` <br> `>=` <br> `!=` <br> `LT` <br> `GT` <br> `Q` <br> `EQ` <br> `LE` <br> `GE` <br> `NE` <br> `#=` <br> `#<` <br> `#>` | Numless <br> Numgreater <br> Numequal <br> Numlesseq <br> Numgreatereq <br> Numnotequal <br> Strless <br> Strgreater <br> Strequal <br> Strequal <br> Strlesseq <br> Strgreatereq <br> Strnotequal <br> Lenequal <br> Lenless <br> Lengreater | `1` <br> `1` <br> `1` <br> `1` <br> `1` <br> `1` <br> `1` <br> `1` <br> `1` <br> `1` <br> `1` <br> `1` <br> `1` <br> `1` <br> `1` <br> `1` | No <br> No <br> No <br> No <br> No <br> No <br> No <br> No <br> No <br> No <br> No <br> No <br> No <br> No <br> No <br> No | Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes
In and not in | Binary | Left | `N` <br> `IN` <br> `NI` | In <br> In <br> Notin | None <br> None <br> None | No <br> No <br> No | Yes <br> Yes <br> Yes
String/regex conversion | Unary | – | `RP` <br> `ST` <br> `X` | Repr <br> Str <br> Regex | None <br> None <br> None | No <br> No <br> No | No <br> No <br> Yes
Low-precedence list operators | Binary | Left | `CB` | Combinations | `[]` | No | Yes
Low-precedence list operators | Unary | – | `MX` <br> `MN` <br> `RC` <br> `SN` <br> `SS` <br> `UQ` <br> `EN` <br> `PM` | Max <br> Min <br> Randchoice <br> Sortnum <br> Sortstring <br> Unique <br> Enumerate <br> Permutations | None <br> None <br> None <br> None <br> None <br> None <br> None <br> None | No <br> No <br> No <br> No <br> No <br> No <br> No <br> No | Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes
Append and push | Binary | Left | `AE` <br> `AL` <br> `PE` <br> `PU` <br> `PB` | Appendelem <br> Appendlist <br> Prependelem <br> Push <br> Pushback | `[]` <br> `[]` <br> `[]` <br> `[]` <br> `[]` | No <br> No <br> No <br> No <br> No | Yes <br> Yes <br> Yes <br> Yes <br> Yes
Pop and dequeue | Unary | – | `PO` <br> `DQ` | Pop <br> Dequeue | None <br> None | No <br> No | Yes <br> Yes
High-precedence list operators | Binary | Left | `^` <br> `^@` <br> `@?` <br> `@*` <br> `<>` <br> `J` <br> `JW` <br> `RL` <br> `Z` <br> `ZD` <br> `WV` <br> `CP` <br> `CG` <br> `ZG` | Split <br> Splitat <br> Find <br> Findall <br> Group <br> Join <br> Joinwrap <br> Repeatlist <br> Zip <br> Zipdefault <br> Weave <br> Cartesianproduct <br> Coordinategrid <br> Zerogrid | `[]` <br> `[]` <br> None <br> `[]` <br> `[]` <br> `""` <br> None <br> `[]` <br> `[]` <br> `[]` <br> `[]` <br> `[]` <br> None <br> None | Both <br> No <br> No <br> No <br> No <br> No <br> No <br> No <br> No <br> No <br> No <br> No <br> Both <br> Both | Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes
Replace-at | Ternary | Left | `RA` | Replaceat | None | No | Yes
High-precedence list operators | Unary | – | `^` <br> `J` <br> `RV` <br> `Z` <br> `ZD` <br> `WV` <br> `CP` <br> `CG` <br> `ZG` <br> `EY` | Split <br> Join <br> Reverse <br> Zip <br> Zipdefault <br> Weave <br> Cartesianproduct <br> Coordinategrid <br> Zerogrid <br> Identitymatrix | None <br> None <br> None <br> None <br> None <br> None <br> None <br> None <br> None <br> None | Both <br> No <br> No <br> No <br> No <br> No <br> No <br> Both <br> Both <br> Both | Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes
Replace | Ternary | Left | `R` | Replace | None | No | No
Wrap | Binary | Left | `WR` | Wrap | `""` | No | Yes
Concatenate | Binary | Left | `.` | Cat | `""` | Both | Yes
Regex modifiers | Unary | – | `.` <br> `K` | Dot <br> Kleenestar | None <br> None | List <br> List | Yes <br> Yes
Remove | Binary | Left | `RM` | Remove | `""` | No | Yes
String repetition | Binary | Left | `X` | Strmul | `""` | Both | Yes
Strip and trim | Binary | Left | `||` <br> `|>` <br> `<|` <br> `TM` | Strip <br> Lstrip <br> Rstrip <br> Trim | `""` <br> `""` <br> `""` <br> `""` | List <br> List <br> List <br> List | Yes <br> Yes <br> Yes <br> Yes
Strip and trim | Unary | – | `||` <br> `|>` <br> `<|` <br> `TM` <br> `LC` <br> `UC` | Strip <br> Lstrip <br> Rstrip <br> Trim <br> Lowercase <br> Uppercase | None <br> None <br> None <br> None <br> None <br> None | List <br> List <br> List <br> List <br> List <br> List | Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes
Range and to-base | Binary | Left | `,` <br> `RR` <br> `TB` | Range <br> Randrange <br> Tobase | None <br> `0` <br> `0` | Both <br> Both <br> Both | Yes <br> Yes <br> Yes
Range and to-base | Unary | – | `,` <br> `RR` <br> `TB` | Rangeto <br> Randrangeto <br> Tobase | None <br> None <br> None | Both <br> Both <br> Both | Yes <br> Yes <br> Yes
Low-precedence numeric operators | Binary | Left | `BA` <br> `BO` <br> `BX` <br> `AT` <br> `CM` | Bitwiseand <br> Bitwiseor <br> Bitwisexor <br> Arctan <br> Numcmp | `-1` <br> `0` <br> `0` <br> None <br> `0` | Both <br> Both <br> Both <br> Both <br> No | Yes <br> Yes <br> Yes <br> Yes <br> Yes
Low-precedence numeric operators | Unary | – | `BN` | Bitwisenot | None | Both | Yes
Addition and subtraction | Binary | Left | `+` <br> `-` | Add <br> Sub | `0` <br> `0` | List <br> List | Yes <br> Yes
Multiplication and division | Binary | Left | `*` <br> `/` <br> `%` <br> `//` | Mul <br> Div <br> Mod <br> Intdiv | `1` <br> `1` <br> `0` <br> `1` | Both <br> Both <br> Both <br> Both | Yes <br> Yes <br> Yes <br> Yes
Unary arithmetic operators | Unary | – | `+` <br> `-` <br> `/` | Pos <br> Neg <br> Invert | None <br> None <br> None | List <br> Both <br> Both | Yes <br> Yes <br> Yes
Exponentiation | Binary | Right | `**` <br> `RT` | Pow <br> Root | `1` <br> `1` | Both <br> Both | Yes <br> Yes
High-precedence numeric operators | Unary | – | `RT` <br> `SI` <br> `CO` <br> `TA` <br> `SE` <br> `CS` <br> `CT` <br> `AT` <br> `RD` <br> `DG` | Sqrt <br> Sine <br> Cosine <br> Tangent <br> Secant <br> Cosec <br> Cotan <br> Arctan <br> Radians <br> Degrees | None <br> None <br> None <br> None <br> None <br> None <br> None <br> None <br> None <br> None | Both <br> Both <br> Both <br> Both <br> Both <br> Both <br> Both <br> Both <br> Both <br> Both | Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes
From-base | Binary | Left | `FB` | Frombase | `0` | Both | Yes
Highest-precedence operators | Binary | Left | `@` <br> `@<` <br> `@>` | At <br> Leftof <br> Rightof | None <br> None <br> None | No <br> No <br> No | Yes <br> Yes <br> Yes
Highest-precedence operators | Unary | – | `@<` <br> `@>` <br> `++` <br> `--` <br> `#` <br> `A` <br> `C` <br> `AB` <br> `SG` <br> `FB` | Leftof <br> Rightof <br> Inc <br> Dec <br> Len <br> Asc <br> Chr <br> Abs <br> Sign <br> Frombase | None <br> None <br> None <br> None <br> None <br> None <br> None <br> None <br> None <br> None | No <br> No <br> No <br> No <br> No <br> Both <br> Both <br> Both <br> Both <br> Both | Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes <br> Yes