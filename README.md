# GORBLANG

## 3rd attempt at a custom programming language

Sample Source
```
fn void main() {
  i32 x = 0;

  if x == 0  x = 1;
  else       x = 2;

  x = 3;
}
```

IR
```
main:
  x = alloca i32
  t0:i32 = const 0
  store t0 -> x
  t1 <- load x
  t2:i32 = const 0
  t3:bool = cmp eq t1 t2
  br t3 main.1 main.2
main.1:
  t4:i32 = const 1
  store t4 -> x
  br main.3
main.2:
  t5:i32 = const 2
  store t5 -> x
  br main.3
main.3:
  t6:i32 = const 3
  store t6 -> x
```

Control Flow Graph Visualization (GraphViz)
<br>
![CFG Visualization](https://raw.githubusercontent.com/loganstottle/gorb/refs/heads/master/main.svg)

LLIR (WIP)
```
main:
  mov v0, 0
  mov frame_index(0), v0
  mov v1, frame_index(0)
  mov v2, 0
  cmp v1, v2
  sete v3
  test v3, v3
  jz main.2
  jmp main.1

main.1:
  mov v4, 1
  mov frame_index(0), v4
  jmp main.3

main.2:
  mov v5, 2
  mov frame_index(0), v5
  jmp main.3

main.3:
  mov v6, 3
  mov frame_index(0), v6
```
