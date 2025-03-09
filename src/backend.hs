module Backend (State, Env (Dict), empty, evalExpr, newExpr, runState, parseAll) where

import Control.Exception (evaluate)
import Data.Map (Map, delete, empty, insert, (!))
import Debug.Trace
import Frontend (parseAll)
import Grammar
  ( Expr (Assign, Block, Boolean, Call, Compare, Func, If, Let, Minus, Number, Return, Sum, Var, CallBlock),
  )
  

data Numeric = Float | Int

type VarIndex = [String]

type VarValue = [Int]

data Env = Dict {vars :: Map String Int, func :: Map String ([String], Expr)} deriving (Show)

type Envs = [Env]

type GlobalEnv = Env

type Context = ([Env], GlobalEnv)

newtype State o a = State {runState :: o -> Maybe (a, o)}

instance Functor (State o) where
  fmap :: (a -> b) -> State o a -> State o b
  fmap f (State s) = State $ \x -> do
    (a', v) <- s x
    return (f a', v)

instance Applicative (State o) where
  pure :: a -> State o a
  pure x = State $ \v -> Just (x, v)

  (<*>) :: State o (a -> b) -> State o a -> State o b
  State a <*> State b = State $ \v -> do
    (f, v') <- a v
    (a', v'') <- b v'
    return (f a', v'')

instance Monad (State o) where
  State s >>= f = State $ \v -> do
    (a, v') <- s v
    (a', v'') <- (runState $ f a) v'
    return (a', v'')

-----------------
-- Env helpers --
-----------------

-- Misc --

applyGlobalEnv :: (Env -> Env) -> Context -> Context
applyGlobalEnv f (locals, global) = (locals, f global)

applyLocalEnv :: (Env -> Env) -> Context -> Context
applyLocalEnv f (local:others, global) = (f local:others, global)

getLocalEnv :: Context -> Env
getLocalEnv (env:_, _) = env

getGlobalEnv :: Context -> Env
getGlobalEnv (_, global) = global

-- Env modifiers --

setVar :: String -> Int -> Env -> Env
setVar name value (Dict m fn) = Dict (insert name value m) fn

delVar :: String -> Env -> Env
delVar name (Dict m fn) = Dict (delete name m) fn

setFn :: String -> [String] -> Expr -> Env -> Env
setFn name args expr (Dict v f) = Dict v (insert name (args, expr) f)

-- Env getters --

getVar :: String -> Env -> Int
getVar s (Dict d _) =  d ! s

getVarFn :: String -> Env -> ([String], Expr)
getVarFn s (Dict _ fn) = fn ! s

-- Env actions --

get :: String -> State Context Int
get s = State $ \context -> do
  return (getVar s (getLocalEnv context) , context)

getFn :: String -> State Context ([String], Expr)
getFn s = State $ \context -> do
  return (getVarFn s (getGlobalEnv context), context)

assign :: String -> Int -> State Context Int
assign s value = State $ \context -> do
  return (value, applyLocalEnv (setVar s value) context)

del :: String -> State Context Int
del s = State $ \context -> do
  return (0, applyLocalEnv (delVar s) context)

define :: String -> [String] -> Expr -> State Context Int
define name args expr = State $ \context -> do
  return (0, applyGlobalEnv (setFn name args expr) context)

getEnv :: State Envs Envs
getEnv = State $ \s ->
  Just (s, s)

evalAllExpr :: [Expr] -> State Context [Int]
evalAllExpr = mapM evalExpr

setUpEnv :: [Int] -> [String] -> State Context Int
setUpEnv [] [] = return 0
setUpEnv (i : is) (s : ss) = do
  assign s i
  setUpEnv is ss

delEnv :: Envs -> Envs
delEnv (e:others) = others

addEnv :: Envs -> Envs
addEnv envs = Dict empty empty : envs

delLocalEnv :: Context -> Context
delLocalEnv (locals, global) = (delEnv locals, global)

addLocalEnv :: Context -> Context
addLocalEnv (locals, global) = (addEnv locals, global)

destroyLocalEnv :: State Context Int
destroyLocalEnv = State $ \context ->
  Just (0, delLocalEnv $ trace ("Exiting context : " ++ show context) context)

createLocalEnv :: State Context Int
createLocalEnv = State $ \context ->
  Just (0, addLocalEnv context)

returnCall :: Int -> State Context Int
returnCall v = do
  destroyLocalEnv
  return v

-----------------
-- Interpreter --
-----------------

evalExpr :: Expr -> State Context Int
evalExpr (Number n) = return n
evalExpr (Boolean b) = return $ fromEnum b
evalExpr (Var ident) = get ident
evalExpr (Let ident expr) = evalExpr expr >>= assign ident
evalExpr (Sum a b) = ((+) <$> evalExpr a) <*> evalExpr b
evalExpr (Minus a b) = ((-) <$> evalExpr a) <*> evalExpr b
evalExpr (Compare a b) = fromEnum <$> (((==) <$> evalExpr a) <*> evalExpr b)
evalExpr (Assign ident expr) = evalExpr expr >>= assign ident
evalExpr (Func ident args expr) = define ident args expr

evalExpr (If a b c) = do
  res <- evalExpr a
  case res of
    0 -> evalExpr c
    1 -> evalExpr b

evalExpr (Block []) = return 0
evalExpr (Block (e : exprs)) = do
      evalExpr e
      evalExpr $ Block exprs

evalExpr (CallBlock []) = return 0
evalExpr (CallBlock (e : exprs)) = do
  case e of
    Return e -> do
      evalExpr e
    _ -> do
      evalExpr e
      evalExpr $ CallBlock exprs

evalExpr (Call ident args) = do
  (idents, exprs) <- wrapBadType ident
  evaluatedArgs <- evalAllExpr args
  createLocalEnv
  setUpEnv evaluatedArgs (trace (" Setting up idents : " ++ show idents) idents)
  v <- evalExpr $ CallBlock exprs
  returnCall v

-- Pseudo Error Handling --

wrapBadType :: String -> State Context ([String], [Expr])
wrapBadType ident = State $ \s ->
  case runState (getFn ident) s of
    Just ((idents, CallBlock b), s') -> Just ((idents, b), s')
    _ -> Nothing

newExpr :: State Context Int -> Int -> State Context Int
newExpr s _ = s