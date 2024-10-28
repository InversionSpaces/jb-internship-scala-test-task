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

## Task 2

### 1. Data Structure

Data structure for `TL` language is defined in [Task2/TLExpr.scala](src/main/scala/task2/TLExpr.scala).

### 2. Equality

Equality for expressions is implemented in `Eq` instance for `TLExpr` in the companion object.
So `left === right` checks if `left` and `right` are structurally equal.

### 3. To String Conversion

Conversion to string is implemented in `Show` instance for `TLExpr` in the companion object.
So `expr.show` returns a string representation of `expr`.

### 4. Parsing

I didn't know if it was allowed to use external libraries for parsing, so I implemented basic
recursive descent parser in [Task2/Parser.scala](src/main/scala/task2/Parser.scala).

Parser for `TLExpr` is defined by `TLExpr.parser` in the companion object. 
Method `TLExpr.parse` implements parsing of a string to `TLExpr` and returns either parsing error or parsed expression.

### 5. Replace

It was unclear if replacement should take place if previous replacement caused the pattern to appear again.
As it was easier to implement, I decided that it should be the case =)

Method `TLExpr.replace` implements replacement functionality for `TLExpr`.

## Tests

Both tasks have tests. To run them use `sbt test`.