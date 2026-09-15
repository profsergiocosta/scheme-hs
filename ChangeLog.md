# Changelog for scheme-hs

## Unreleased changes

Completed the interpreter through a working REPL with variables,
functions, and closures, following "Write Yourself a Scheme in 48
Hours":

- **More primitives**: numeric/string comparisons (`=`, `<`, `>`,
  `<=`, `>=`, `/=`, `string=?`, `string<?`, ...), booleans (`&&`,
  `||`, `not`), list operations (`car`, `cdr`, `cons`), equality
  (`eq?`, `eqv?`, `equal?`), and type predicates (`null?`, `pair?`,
  `list?`, `symbol?`, `string?`, `number?`, `boolean?`).
- **Conditionals**: `if` and `cond` (including the `else` clause and
  the "cond as or" no-body-clause form).
- **Variables and mutation**: `define` and `set!`, backed by a
  mutable `Env = IORef [(String, IORef LispVal)]`. This changed
  `eval`'s type from `LispVal -> ThrowsError LispVal` (pure) to
  `Env -> LispVal -> IOThrowsError LispVal`
  (`IOThrowsError = ExceptT LispError IO`), since variable lookups
  now need IO.
- **Functions**: `lambda` (fixed arity, variadic `(lambda (a . rest)
  ...)`, and fully variadic `(lambda args ...)`), closures (a `Func`
  captures the environment it was created in), and the `(define (f
  x) ...)` shorthand for naming a function.
- **A real REPL**: `scheme-hs-exe` with no arguments now starts a
  read-eval-print loop (`quit` to exit) that keeps state (`define`d
  variables and functions) across inputs, instead of evaluating a
  single command-line argument and exiting.
- **Tests**: `test/Spec.hs` now has 26 cases covering every stage
  above, plus error handling (unbound variables, wrong argument
  counts, calling a non-function all produce a readable message
  through `evalString` rather than crashing).

The parser (`src/Parser.hs`) needed no changes at all for any of
this — `if`, `define`, `lambda`, `cond` are all just s-expressions
(`List [Atom "if", ...]`); dispatch on the special form happens in
`eval`, not in parsing.
