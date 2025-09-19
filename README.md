# GORBLANG

* 4th attempt at a custom programming language
* Making this to learn Zig

### PREVIEW
```
$ ./gorb test.src
Source: 1 + 2 / (x - 3)

Tokens:
  { .NumberLiteral "1" }
  { .Plus "+" }
  { .NumberLiteral "2" }
  { .Slash "/" }
  { .OpenParen "(" }
  { .Identifier "x" }
  { .Minus "-" }
  { .NumberLiteral "3" }
  { .CloseParen ")" }
```

### TODO
  * multi-character symbols (+=, &&, etc)
  * fixed width float & integer types, bools, pointers
  * generics (monomorphization)
  * composite types (structs, tuples, arrays, sets, enums, unions, variants)
  * automatic reference counting
  * preemptive green threads
