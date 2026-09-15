{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module LispVal where

import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Text.ParserCombinators.Parsec hiding (spaces)

-- * Errors

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected " ++ show expected
    ++ " args; found values "
    ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected
    ++ ", found "
    ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (Default message) = message

instance Show LispError where show = showError

-- | Pure computations that can fail (parsing, primitive application).
type ThrowsError = Either LispError

-- | Computations that can fail AND need IO (environment lookups,
-- since variables live behind 'IORef's).
type IOThrowsError = ExceptT LispError IO

-- | Works for both 'ThrowsError' and 'IOThrowsError' -- both are
-- instances of 'MonadError' 'LispError'.
trapError :: MonadError LispError m => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

-- * Environments

-- | An environment is a mutable association list: each variable is
-- its own 'IORef', so 'set!' can mutate a binding in place without
-- rebuilding the whole environment, and closures that share an
-- environment see each other's mutations.
type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . flip writeIORef value)
    (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindingsToAdd env = fmap (++ env) (mapM addBinding bindingsToAdd)
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)

-- * Values

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func
      { params :: [String],
        vararg :: Maybe String,
        body :: [LispVal],
        closure :: Env
      }

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
showVal (PrimitiveFunc _) = "<primitive procedure>"
showVal Func {params = args, vararg = varargs} =
  "(lambda (" ++ unwords args
    ++ ( case varargs of
           Nothing -> ""
           Just arg -> " . " ++ arg
       )
    ++ ") ...)"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

-- * Evaluator

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval env (Atom aid) = getVar env aid
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", predicate, conseq, alt]) = do
  result <- eval env predicate
  case result of
    Bool False -> eval env alt
    _ -> eval env conseq
eval env (List (Atom "cond" : clauses)) = evalCond env clauses
eval env (List (Atom "begin" : exprs)) = evalBody env exprs
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : ps) : bodyExprs)) =
  makeNormalFunc env ps bodyExprs >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : ps) varargs : bodyExprs)) =
  makeVarArgs varargs env ps bodyExprs >>= defineVar env var
eval env (List (Atom "lambda" : List ps : bodyExprs)) =
  makeNormalFunc env ps bodyExprs
eval env (List (Atom "lambda" : DottedList ps varargs : bodyExprs)) =
  makeVarArgs varargs env ps bodyExprs
eval env (List (Atom "lambda" : varargs@(Atom _) : bodyExprs)) =
  makeVarArgs varargs env [] bodyExprs
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- | @(cond (test expr...) ... (else expr...))@. A clause with no
-- body (just a test) evaluates to the test's own value if it's
-- truthy -- the R5RS "cond as an or" idiom.
evalCond :: Env -> [LispVal] -> IOThrowsError LispVal
evalCond _ [] = throwError $ Default "No matching clause in cond"
evalCond env (List (Atom "else" : exprs) : _) = evalBody env exprs
evalCond env (List (test : exprs) : rest) = do
  result <- eval env test
  case result of
    Bool False -> evalCond env rest
    _ -> if null exprs then return result else evalBody env exprs
evalCond _ (badClause : _) = throwError $ BadSpecialForm "Malformed cond clause" badClause

-- | Evaluates a sequence of expressions, returning the value of the
-- last one -- the semantics of both @begin@ and a function body.
evalBody :: Env -> [LispVal] -> IOThrowsError LispVal
evalBody env exprs = fmap last (mapM (eval env) exprs)

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env ps bodyExprs = return $ Func (map showVal ps) varargs bodyExprs env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . showVal

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func ps varargs bodyExprs env) args =
  if num ps /= num args && varargs == Nothing
    then throwError $ NumArgs (num ps) args
    else liftIO (bindVars env $ zip ps args) >>= bindVarArgs varargs >>= flip evalBody bodyExprs
  where
    remainingArgs = drop (length ps) args
    num = toInteger . length
    bindVarArgs arg env' = case arg of
      Just argName -> liftIO $ bindVars env' [(argName, List remainingArgs)]
      Nothing -> return env'
apply notFunc _ = throwError $ NotFunction "Not a function" (show notFunc)

-- * Primitives

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map makePrimitiveFunc primitives)
  where
    makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("not", lispNot),
    ("string=?", strBoolBinop (==)),
    ("string<?", strBoolBinop (<)),
    ("string>?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<=)),
    ("string>=?", strBoolBinop (>=)),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eq?", eqv),
    ("eqv?", eqv),
    ("equal?", equal),
    ("null?", isNull),
    ("pair?", isPair),
    ("list?", isListVal),
    ("symbol?", isSymbol),
    ("string?", isString),
    ("number?", isNumber),
    ("boolean?", isBoolean)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op ps = mapM unpackNum ps >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n
   in if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do
      left <- unpacker $ args !! 0
      right <- unpacker $ args !! 1
      return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

lispNot :: [LispVal] -> ThrowsError LispVal
lispNot [Bool b] = return $ Bool (not b)
lispNot [_] = return $ Bool False -- everything non-#f is truthy, so (not anything-else) is #f
lispNot badArgs = throwError $ NumArgs 1 badArgs

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List (x : xs)
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

-- | Shallow, type-exact equality -- the R5RS @eqv?@ (and @eq?@,
-- treated here as a synonym, since scheme-hs has no mutable pairs to
-- tell them apart).
eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2] =
  return $ Bool $ (length arg1 == length arg2) && all eqvPair (zip arg1 arg2)
  where
    eqvPair (x1, x2) = case eqv [x1, x2] of
      Left _ -> False
      Right (Bool val) -> val
      Right _ -> False
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

-- | Deep, coercing equality -- the R5RS @equal?@. Recurses into
-- lists/dotted lists, and (like the tutorial's own weak typing
-- elsewhere) treats numbers and their string representation as equal
-- to each other.
equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = return $ Bool (valsEqual arg1 arg2)
  where
    valsEqual (Number a) (Number b) = a == b
    valsEqual (String a) (String b) = a == b
    valsEqual (Bool a) (Bool b) = a == b
    valsEqual (Atom a) (Atom b) = a == b
    valsEqual (List a) (List b) = length a == length b && and (zipWith valsEqual a b)
    valsEqual (DottedList xs x) (DottedList ys y) = valsEqual (List xs) (List ys) && valsEqual x y
    valsEqual (Number a) (String b) = show a == b
    valsEqual (String a) (Number b) = a == show b
    valsEqual _ _ = False
equal badArgList = throwError $ NumArgs 2 badArgList

isNull :: [LispVal] -> ThrowsError LispVal
isNull [List []] = return $ Bool True
isNull [_] = return $ Bool False
isNull badArgList = throwError $ NumArgs 1 badArgList

isPair :: [LispVal] -> ThrowsError LispVal
isPair [List (_ : _)] = return $ Bool True
isPair [DottedList _ _] = return $ Bool True
isPair [_] = return $ Bool False
isPair badArgList = throwError $ NumArgs 1 badArgList

isListVal :: [LispVal] -> ThrowsError LispVal
isListVal [List _] = return $ Bool True
isListVal [_] = return $ Bool False
isListVal badArgList = throwError $ NumArgs 1 badArgList

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [Atom _] = return $ Bool True
isSymbol [_] = return $ Bool False
isSymbol badArgList = throwError $ NumArgs 1 badArgList

isString :: [LispVal] -> ThrowsError LispVal
isString [String _] = return $ Bool True
isString [_] = return $ Bool False
isString badArgList = throwError $ NumArgs 1 badArgList

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [Number _] = return $ Bool True
isNumber [_] = return $ Bool False
isNumber badArgList = throwError $ NumArgs 1 badArgList

isBoolean :: [LispVal] -> ThrowsError LispVal
isBoolean [Bool _] = return $ Bool True
isBoolean [_] = return $ Bool False
isBoolean badArgList = throwError $ NumArgs 1 badArgList