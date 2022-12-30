# L programming language & static analyzer

## L Syntax

L is a simple imperative programming language which supports basic statements:

* Variable assignment: `<name> := <expr>`
* `if`:

```text
if a > b then { write a; b := a  } else { write b; a := b }
```

That generally is

```text
if <expr> then { <trueBody> } else { <falseBody> }
```

* `while`:

```text
while x > 10 do { write x; x := x - 1 }
```

That generally is

```text
while <expr> do { <body> }
```

* `Skip` -- no-op command
* `Write <expr>` and `Read <variable name>` commands
* Functions:
    * where declaration `def <name>(<arg1>, <arg2>,...) { <body> }`
      or `def <name>(<arg1>, <arg2>,...) { <body>} return <expr>`
    * and call is `<name>(arg1, arg2,...)` where `arg` is an expression. Call also supported in
      expressions: `x := 2 + sum(2, 3)`.

Expressions support variable access, function calls and several binary operations
such: `+, -, *, /, %, >, >=, ==, !=, <=, <, &&, ||`.

### Examples

Factorial function:

```text
def f(x) { ans := 1; while (x > 0) do { ans := ans * x; x := x - 1 } } return ans
```

Recursive Fibonacci numbers:

```text
def fib(x) { ans := 1; if (x < 2) then { skip } else { ans := fib(x - 1) + fib(x - 2) } } return ans 
```

## Usage of CLI app

### REPL

If you run the application without arguments, you will get a REPL mode.

### File interpretation

If flag `-i <FILE>` is passed, a file will be interpreted. Additionally, you can provide `--liveness-optimization` flag
to enable liveness optimization and provide starting variables and its values.

## Static Analysis

We wrote a static analyzer with liveness optimization that eliminate dead code. It could make code faster in situations
when we compute unused in future code. E.g. in this code we have unnecessary infinity loop:

```text
write 1
def f() { x := 0; while 1 do { x := x + 1} } return 1
def g() { x := f() }
g()
```

This will be optimized into

```text
write 1
def f() { x := 0; while 1 do { x := x + 1} } return 1
def g() { }
g()
```

That is in common not used for optimization, but used for error analysis and highlighting.
