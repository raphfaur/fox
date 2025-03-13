{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Backend (State, Env (Dict), empty, evalExpr, newExpr, runState, parseAll) where

import Data.Map (Map, delete, empty, insert, lookup)
import Debug.Trace
import Frontend (parseAll)
import Grammar
  ( Expr (Assign, Block, Boolean, Call, CallBlock, Compare, Func, If, Let, Minus, Number, Return, Sum, Var),
  )

data Env = Dict {vars :: Map String Int, func :: Map String ([String], Grammar.Expr)} deriving (Show)

type Envs = [Env]

type GlobalEnv = Env

type Context = ([Env], GlobalEnv)

newtype State o a = State {runState :: o -> Either RuntimeError (a, o)}

instance Functor (State o) where
  fmap f (State s) = State $ \x -> do
    (a', v) <- s x
    return (f a', v)

instance Applicative (State o) where
  pure x = State $ \v -> pure (x, v)

  State a <*> State b = State $ \v -> do
    (f, v') <- a v
    (a', v'') <- b v'
    return (f a', v'')

instance Monad (State o) where
  State s >>= f = State $ \v -> do
    (a, v') <- s v
    (a', v'') <- (runState $ f a) v'
    return (a', v'')

--------------------
-- Error handling --
--------------------

data RuntimeError = NameError String | ScopeError String

instance Show RuntimeError where
  show (NameError e) = color cRed "*** NameError ***\n" ++ e ++ color cRed "\n*** NameError ***"
  show (ScopeError e) = "ScopeError"

raise :: a -> Either a b
raise = Left

type Color = String

cRed :: String
cRed = "\ESC[91m"

cBlue :: String
cBlue = "\ESC[94m"

cDefault :: String
cDefault = "\ESC[0m"

color :: Color -> String -> String
color c s = c ++ s ++ cDefault

-----------------
-- Env helpers --
-----------------

-- Misc --

applyGlobalEnv :: (Env -> Env) -> Context -> Context
applyGlobalEnv f (locals, global) = (locals, f global)

applyLocalEnv :: (Env -> Env) -> Context -> Context
applyLocalEnv f (local : others, global) = (f local : others, global)

getLocalEnv :: Context -> Env
getLocalEnv (env : _, _) = env

getGlobalEnv :: Context -> Env
getGlobalEnv (_, global) = global

-- Env modifiers --

setVar :: String -> Int -> Env -> Env
setVar name value (Dict m fn) = Dict (insert name value m) fn

delVar :: String -> Env -> Env
delVar name (Dict m fn) = Dict (delete name m) fn

setFn :: String -> [String] -> Grammar.Expr -> Env -> Env
setFn name args expr (Dict v f) = Dict v (insert name (args, expr) f)

-- Env getters --

getVar :: String -> Env -> Maybe Int
getVar s (Dict d _) = Data.Map.lookup s d

getVarFn :: String -> Env -> Maybe ([String], Grammar.Expr)
getVarFn s (Dict _ fn) = Data.Map.lookup s fn

-- Env actions --

get :: String -> State Context Int
get s = State $ \context -> do
  case getVar s (getLocalEnv context) of
    Just v -> return (v, context)
    Nothing -> raise $ NameError ("Unknwon variable " ++ s)

getFn :: String -> State Context ([String], Grammar.Expr)
getFn s = State $ \context -> do
  case getVarFn s (getGlobalEnv context) of
    Just f -> return (f, context)
    Nothing -> raise $ NameError ("Unknwown function " ++ color cBlue s)

assign :: String -> Int -> State Context Int
assign s value = State $ \context -> do
  return (value, applyLocalEnv (setVar s value) context)

del :: String -> State Context Int
del s = State $ \context -> do
  return (0, applyLocalEnv (delVar s) context)

define :: String -> [String] -> Grammar.Expr -> State Context Int
define name args expr = State $ \context -> do
  return (0, applyGlobalEnv (setFn name args expr) context)

getEnv :: State Envs Envs
getEnv = State $ \s ->
  pure (s, s)

evalAllExpr :: [Grammar.Expr] -> State Context [Int]
evalAllExpr = mapM evalExpr

setUpEnv :: [Int] -> [String] -> State Context Int
setUpEnv [] [] = return 0
setUpEnv (i : is) (s : ss) = do
  assign s i
  setUpEnv is ss

delEnv :: Envs -> Envs
delEnv (_ : others) = others

addEnv :: Envs -> Envs
addEnv envs = Dict empty empty : envs

delLocalEnv :: Context -> Context
delLocalEnv (locals, global) = (delEnv locals, global)

addLocalEnv :: Context -> Context
addLocalEnv (locals, global) = (addEnv locals, global)

destroyLocalEnv :: State Context Int
destroyLocalEnv = State $ \context ->
  pure (0, delLocalEnv $ trace ("Exiting context : " ++ show context) context)

createLocalEnv :: State Context Int
createLocalEnv = State $ \context ->
  pure (0, addLocalEnv context)

returnCall :: Int -> State Context Int
returnCall v = do
  destroyLocalEnv
  return v

-----------------
-- Interpreter --
-----------------

evalExpr :: Grammar.Expr -> State Context Int
evalExpr (Grammar.Number n) = return n
evalExpr (Grammar.Boolean b) = return $ fromEnum b
evalExpr (Grammar.Var ident) = get ident
evalExpr (Grammar.Let ident expr) = evalExpr expr >>= assign ident
evalExpr (Grammar.Sum a b) = ((+) <$> evalExpr a) <*> evalExpr b
evalExpr (Grammar.Minus a b) = ((-) <$> evalExpr a) <*> evalExpr b
evalExpr (Grammar.Compare a b) = fromEnum <$> (((==) <$> evalExpr a) <*> evalExpr b)
evalExpr (Grammar.Assign ident expr) = evalExpr expr >>= assign ident
evalExpr (Grammar.Func ident args expr) = define ident args expr
evalExpr (Grammar.If a b c) = do
  res <- evalExpr a
  case res of
    0 -> evalExpr c
    1 -> evalExpr b
evalExpr (Grammar.Block []) = return 0
evalExpr (Grammar.Block (e : exprs)) = do
  evalExpr e
  evalExpr $ Grammar.Block exprs
evalExpr (Grammar.CallBlock []) = return 0
evalExpr (Grammar.CallBlock (e : exprs)) = do
  case e of
    Grammar.Return e -> do
      evalExpr e
    _ -> do
      evalExpr e
      evalExpr $ Grammar.CallBlock exprs
evalExpr (Grammar.Call ident args) = do
  (idents, exprs) <- wrapBadType ident
  evaluatedArgs <- evalAllExpr args
  createLocalEnv
  setUpEnv evaluatedArgs (trace (" Setting up idents : " ++ show idents) idents)
  v <- evalExpr $ Grammar.CallBlock exprs
  returnCall v

-- Pseudo Error Handling --

wrapBadType :: String -> State Context ([String], [Grammar.Expr])
wrapBadType ident = State $ \s ->
  case runState (getFn ident) s of
    Right ((idents, Grammar.CallBlock b), s') -> pure ((idents, b), s')
    Left e -> raise e

newExpr :: State Context Int -> Int -> State Context Int
newExpr s _ = s
