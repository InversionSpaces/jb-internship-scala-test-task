# Solution for JB scala internship test task

## Task 1

```
f(x) = f(x - 1) + f(x - 1)
f(0) = 1
```

### Naive implementation

Naive implementation of `f` has complexity `O(2^x)` 
because it traverses a full binary tree of depth `x` which has `2^x - 1` nodes.

### Fast pow algorithm

Prove by induction that `f(x) = 2^x`:
```
Base: f(0) = 1 = 2^0
Step: f(x + 1) = f(x) + f(x) = 2 * f(x) = 
[by induction hypothesis]
= 2 * 2^x = 2^(x + 1)
```

So `f(x)` can be computed in `O(log(x))` time using fast pow algorithm.

### Bit shift implementation

Function `f` can be implemented with `O(1)` complexity
utilizing bit shift operation as `f(x) = 2^x = 1 << x`.

All implementations of `f` are in [Task1](src/main/scala/task1/package.scala).