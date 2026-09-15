module Main (main) where

import LispVal
import Parser (readExpr)
import System.Exit (exitFailure)

type Case = (String, IO Bool)

-- | Runs one expression against a fresh environment (with all
-- primitives bound) and returns its printed representation, exactly
-- as the REPL would show it -- errors included, since evalString
-- never throws.
runExpr :: String -> IO String
runExpr expr = do
  env <- primitiveBindings
  evalStringForTest env expr

-- | A local copy of Main's evalString, since it isn't exposed by the
-- executable -- keeps the test suite independent of app/Main.hs.
evalStringForTest :: Env -> String -> IO String
evalStringForTest env expr =
  runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

check :: String -> String -> IO Bool
check expr expected = do
  actual <- runExpr expr
  return (actual == expected)

cases :: [Case]
cases =
  [ ("arithmetic: (+ 1 2 3) = 6", check "(+ 1 2 3)" "6")
  , ("arithmetic: nested (* 2 (+ 3 4)) = 14", check "(* 2 (+ 3 4))" "14")
  , ("quote: '(1 2 3)", check "'(1 2 3)" "(1 2 3)")

  -- Stage 1: comparisons, booleans, list primitives
  , ("comparison: (< 1 2) = #t", check "(< 1 2)" "#t")
  , ("comparison: (= 1 2) = #f", check "(= 1 2)" "#f")
  , ("boolean: (&& #t #f) = #f", check "(&& #t #f)" "#f")
  , ("boolean: (not #f) = #t", check "(not #f)" "#t")
  , ("list: (car '(1 2 3)) = 1", check "(car '(1 2 3))" "1")
  , ("list: (cdr '(1 2 3)) = (2 3)", check "(cdr '(1 2 3))" "(2 3)")
  , ("list: (cons 1 '(2 3)) = (1 2 3)", check "(cons 1 '(2 3))" "(1 2 3)")
  , ("list: (null? '()) = #t", check "(null? '())" "#t")
  , ("equality: (equal? '(1 2) '(1 2)) = #t", check "(equal? '(1 2) '(1 2))" "#t")
  , ("equality: (eqv? 'a 'a) = #t", check "(eqv? 'a 'a)" "#t")

  -- Stage 2: conditionals
  , ("if: true branch", check "(if (> 3 2) 'yes 'no)" "yes")
  , ("if: false branch", check "(if (> 2 3) 'yes 'no)" "no")
  , ("cond: matches second clause", check "(cond ((= 1 2) 'a) ((= 1 1) 'b) (else 'c))" "b")
  , ("cond: falls through to else", check "(cond ((= 1 2) 'a) (else 'c))" "c")

  -- Stage 3: variables and mutation
  , ("define + lookup", check "(begin (define x 5) x)" "5")
  , ("set! mutates an existing binding", check "(begin (define x 5) (set! x 10) x)" "10")

  -- Stage 4: functions, recursion, closures
  , ("lambda application", check "((lambda (x y) (+ x y)) 3 4)" "7")
  , ("define function shorthand", check "(begin (define (square x) (* x x)) (square 5))" "25")
  , ("recursion: factorial of 5", check
      "(begin (define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 5))"
      "120")
  , ("closures: shared mutable state across calls", check
      "(begin (define (make-counter) (define n 0) (lambda () (begin (set! n (+ n 1)) n))) (define c1 (make-counter)) (c1) (c1) (c1))"
      "3")
  , ("varargs: a lambda with a rest parameter collects extra args", check
      "((lambda (a . rest) rest) 1 2 3)"
      "(2 3)")

  -- Error handling: evalString never crashes, it returns the error
  -- as a readable string
  , ("errors: unbound variable produces a readable message", check
      "totally-undefined-variable"
      "Getting an unbound variable: totally-undefined-variable")
  , ("errors: wrong arg count to a primitive", check "(+ 1)" "Expected 2 args; found values 1")
  , ("errors: calling a non-function", check "(5 1 2)" "Not a function: \"5\"")
  ]

main :: IO ()
main = do
  results <- mapM report cases
  if and results
    then putStrLn "All tests passed."
    else exitFailure
  where
    report (name, action) = do
      ok <- action
      putStrLn ((if ok then "OK     " else "FAILED ") ++ name)
      pure ok
