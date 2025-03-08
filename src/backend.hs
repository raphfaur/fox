module Backend (State, Variables (Dict), empty, evalExpr, newExpr, runState, parseAll) where

import Control.Exception (evaluate)
import Data.Map (Map, delete, empty, insert, (!))
import Debug.Trace
import Frontend (parseAll)
import Grammar
  ( Expr (Assign, Block, Boolean, Call, Compare, Func, If, Let, Minus, Number, Return, Sum, Var),
  )

data Numeric = Float | Int

type VarIndex = [String]

type VarValue = [Int]

data Variables = Dict {vars :: Map String Int, func :: Map String ([String], Expr)} deriving (Show)

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

delVar :: String -> Variables -> Variables
delVar name (Dict m fn) = Dict (delete name m) fn

setVar :: String -> Int -> Variables -> Variables
setVar name value (Dict m fn) = Dict (insert name value m) fn

getVar :: String -> Variables -> Int
getVar s (Dict d _) = d ! s

getVarFn :: String -> Variables -> ([String], Expr)
getVarFn s (Dict _ fn) = fn ! s

get :: String -> State Variables Int
get s = State $ \v -> do
  return (getVar s v, v)

getFn :: String -> State Variables ([String], Expr)
getFn s = State $ \v -> do
  return (getVarFn s v, v)

assign :: String -> Int -> State Variables Int
assign s value = State $ \v -> do
  return (value, setVar s value v)

del :: String -> State Variables Int
del s = State $ \v -> do
  return (0, delVar s v)

setFn :: String -> [String] -> Expr -> Variables -> Variables
setFn name args expr (Dict v f) = Dict v (insert name (args, expr) f)

define :: String -> [String] -> Expr -> State Variables Int
define name args expr = State $ \v -> do
  return (0, setFn name args expr v)

evalExpr :: Expr -> State Variables Int
evalExpr (Number n) = return n
evalExpr (Boolean b) = return $ fromEnum b
evalExpr (Var ident) = get ident
evalExpr (Let ident expr) = evalExpr expr >>= assign ident
evalExpr (Sum a b) = ((+) <$> evalExpr a) <*> evalExpr b
evalExpr (Minus a b) = ((-) <$> evalExpr a) <*> evalExpr b
evalExpr (Compare a b) = fromEnum <$> (((==) <$> evalExpr a) <*> evalExpr b)
evalExpr (If a b c) = do
  res <- evalExpr a
  case res of
    0 -> evalExpr c
    1 -> evalExpr b
evalExpr (Block []) = return 0
evalExpr (Block (e : exprs)) = do
  case e of
    Return e -> do
      evalExpr e
    _ -> do
      evalExpr e
      evalExpr $ Block exprs
evalExpr (Assign ident expr) = evalExpr expr >>= assign ident
evalExpr (Func ident args expr) = define ident args expr
evalExpr (Call ident args) = do
  (idents, exprs) <- wrapBadType ident
  oldenv <- getEnv
  setUpEnv args idents
  v <- evalExpr $ Block exprs
  unSetEnv idents
  return v

getEnv :: State Variables Variables
getEnv = State $ \s ->
  Just (s, s)

setEnv :: Variables -> State Variables Int
setEnv v = State $ \s ->
  Just (0, v)

wrapBadType :: String -> State Variables ([String], [Expr])
wrapBadType ident = State $ \s ->
  case runState (getFn ident) s of
    Just ((idents, Block b), s') -> Just ((idents, b), s')
    _ -> Nothing

setUpEnv :: [Expr] -> [String] -> State Variables Int
setUpEnv [] [] = return 0
setUpEnv (e : es) (s : ss) = do
  eval <- evalExpr e
  assign s eval
  setUpEnv es ss

unSetEnv :: [String] -> State Variables Int
unSetEnv [] = return 0
unSetEnv (s : ss) = do
  del s
  unSetEnv ss

newExpr :: State Variables Int -> Int -> State Variables Int
newExpr s _ = s