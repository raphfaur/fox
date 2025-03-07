module Backend (State, Variables (Dict), empty , evalExpr, newExpr, runState, parseAll) where

import Grammar
    ( Expr(If, Number, Boolean, Var, Let, Sum, Minus, Compare, Block, Assign) )
import Data.Map (insert, Map, (!), empty)
import Control.Exception (evaluate)
import Frontend (parseAll)

data Numeric = Float | Int

type VarIndex = [String]

type VarValue = [Int]

newtype Variables = Dict {unDict :: Map String Int} deriving (Show)

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

setVar:: String -> Int -> Variables -> Variables
setVar name value (Dict m) = Dict $ insert name value m

getVar:: String -> Variables -> Int
getVar s (Dict d) = d ! s


get:: String -> State Variables Int
get s = State $ \v -> do
    return (getVar s v, v)

assign :: String -> Int -> State Variables Int
assign s value = State $ \v -> do
    return (value, setVar s value v)


evalExpr:: Expr -> State Variables Int
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
evalExpr (Block (e:exprs)) = do 
    evalExpr e
    evalExpr $ Block exprs
evalExpr (Assign ident expr) = evalExpr expr >>= assign ident

newExpr:: State Variables Int -> Int -> State Variables Int
newExpr s _ = s