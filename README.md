# GORBLANG

## 3rd attempt at a custom programming language

Sample Source
```
fn i32 add(i32 a, i32 b) {
  return a + b;
}

fn void main() {
  i32 x = add(1, 2);
}
```

IR
```
add:
  t0:i32 = param a
  t1:i32 = param b
  t2:i32 = add t0 t1
  ret t2

main:
  x = alloca i32
  t0:i32 = const 1
  t1:i32 = const 2
  t2:i32 = call add t0 t1 
  x = store t2
```

LLIR (WIP)
```
add:
  mov v0, rdi
  mov v1, rsi
  mov v2, v0
  add v2, v1
  mov rax, v2

main:
  mov v0, 1
  mov v1, 2
  mov rdi, v0
  mov rsi, v1
  call add
  mov v2, rax
  mov frame_index(0), v2
```
