# GORBLANG

Sample Source:
```
fn void main() {
  mut u32 x = 0;

  if x == 0  x = 1;
  else       x = 2;

  x = 3;
}
```

Emitted IR:
```
main:
  x = alloca
  t0 = const 0
  x = store t0
  t1 = load x
  t2 = const 0
  t3 = cmp t1 == t2
  if t3 br main.1 else main.2
main.1:
  t4 = const 1
  x = store t4
  br main.3
main.2:
  t5 = const 2
  x = store t5
  br main.3
main.3:
  t6 = const 3
  x = store t6
```

Control Flow Graph Visualization (via emitted GraphViz .DOT files):
![CFG Visualization](https://raw.githubusercontent.com/loganstottle/gorb/refs/heads/master/main.svg)

### NEXT UP
* dominator tree (Semi-NCA)
* SSA construction
* analysis and transformation passes over IR
* instruction selection (tree/DAG pattern matching)
* register allocation (graph coloring)
* maybe target specific peephole optimizations
