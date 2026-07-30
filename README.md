# SICP 4.1 Scheme Interpreter

A small Scheme interpreter written in Haskell, following the evaluator from
SICP section 4.1. The reader is built from a parser combinator implementation
in this repository, and evaluation is split into analysis and execution.

## Run

Run a Scheme source file:

```console
cabal run sicp -- program.scm
```

Run the tests:

```console
cabal test
```

Building GHC programs requires the GMP development library on systems where
GHC uses GMP for arbitrary-precision integers.

## Language

The interpreter supports integer, string and boolean literals; quoted data;
lexical closures; `define`, `set!`, `if`, `lambda`, `begin`, and `cond`; proper
and dotted lists; and a compact set of arithmetic, comparison, pair, predicate,
and output primitives.

Internal definitions use simultaneous scope: all names are allocated before
their initializers run, so local procedures may be mutually recursive. Reading
an internal binding before it has been initialized is an error.

Numbers are integers only. `/` performs division truncated toward zero. Lambda
parameter lists have fixed arity. Proper tail-call optimization and the
exercise extensions from SICP 4.1 are intentionally outside this version.
