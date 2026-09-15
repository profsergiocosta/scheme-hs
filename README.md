# scheme-hs

A Scheme interpreter written in Haskell, following ["Write Yourself a
Scheme in 48
Hours"](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours)
(Jonathan Tang), with the environment/closures architecture also
informed by ["Write You A Scheme, Version
2.0"](https://wespiser.com/writings/wyas/00_overview.html).

## What it supports

- **Values**: integers, strings, booleans (`#t`/`#f`), symbols, lists
  `(1 2 3)`, dotted pairs `(1 . 2)`, quoting `'x`.
- **Special forms**: `quote`, `if`, `cond` (with `else`), `define`
  (both `(define x val)` and the function shorthand
  `(define (f x) ...)`), `set!`, `lambda` (fixed arity, variadic with
  a rest parameter, and fully variadic), `begin`.
- **Closures**: a `lambda` captures the environment it was created
  in, so patterns like a counter with private mutable state work:
  ```scheme
  (define (make-counter)
    (define n 0)
    (lambda () (begin (set! n (+ n 1)) n)))
  (define c1 (make-counter))
  (c1) (c1) (c1)  ; => 1, 2, 3
  ```
- **Primitives**: arithmetic (`+ - * / mod quotient remainder`),
  numeric/string comparison (`= < > <= >= /= string=? string<? ...`),
  booleans (`&& || not`), list operations (`car cdr cons`), equality
  (`eq? eqv? equal?`), and type predicates (`null? pair? list? symbol?
  string? number? boolean?`).
- **A REPL** (`quit` to exit) that keeps state across inputs.
- **Readable errors** — a bad program never crashes the interpreter;
  errors (unbound variable, wrong argument count, type mismatch,
  calling a non-function, ...) print as a message and evaluation
  continues.

## What it doesn't support (yet)

- `let`/`let*`/`letrec` (currently: nest `lambda` applications, or use
  internal `define`s inside a function body, as `make-counter` does
  above).
- Vectors, characters, floating-point numbers, tail-call optimization,
  `call/cc`, macros, ports/file I/O, `case`.
- Loading a `.scm` file — only single expressions (or a `begin` of
  several) via the REPL or the command line.

## Build and run

With Cabal:
```sh
cabal build
cabal run scheme-hs-exe              # REPL
cabal run scheme-hs-exe -- '(+ 1 2)' # one expression
cabal test
```

Or directly with GHC, if `parsec`/`mtl`/`containers`/`text` are
already available (e.g. via `apt install ghc libghc-parsec-dev
libghc-mtl-dev` on Debian/Ubuntu — `containers` and `text` ship with
GHC itself):
```sh
ghc -isrc -iapp -o scheme-hs app/Main.hs
./scheme-hs                 # REPL
./scheme-hs '(+ 1 2)'       # one expression

ghc -isrc -itest -o scheme-hs-test test/Spec.hs
./scheme-hs-test
```

## Project layout

```
scheme-hs/
├── app/Main.hs        -- entry point: REPL or single-expression mode
├── src/
│   ├── Parser.hs       -- Parsec grammar, string -> LispVal
│   └── LispVal.hs      -- the LispVal type, the environment, eval/apply, primitives
└── test/Spec.hs        -- 26 test cases
```

`Parser.hs` only ever produces plain s-expressions (`List [Atom
"if", ...]`, etc.) — it has no notion of `if`, `define`, or `lambda`
as special syntax. All of that dispatch lives in `eval`
(`LispVal.hs`), which is what makes adding a new special form a
one-clause change to `eval` rather than a parser change.
