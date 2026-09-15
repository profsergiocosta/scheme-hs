module Main where

import LispVal
import Parser
import System.Environment
import System.IO

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- | Parses and evaluates one expression against the given
-- environment, returning its printed representation (or an error
-- message, also as a string -- errors never throw out of this
-- function, matching how a REPL should behave).
evalString :: Env -> String -> IO String
evalString env expr =
  runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
  result <- prompt
  if predicate result
    then return ()
    else action result >> until_ predicate prompt action

runRepl :: IO ()
runRepl = do
  env <- primitiveBindings
  until_ (== "quit") (readPrompt "scheme-hs>>> ") (evalAndPrint env)

runOne :: String -> IO ()
runOne expr = do
  env <- primitiveBindings
  evalAndPrint env expr

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl
    [expr] -> runOne expr
    _ -> putStrLn "Usage: scheme-hs-exe [expression]  (no args starts a REPL)"
