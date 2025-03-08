{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

import Control.Monad ( Monad, Functor )
import qualified Control.Monad as Control
import qualified Control.Applicative as Control.Monad
import Data.Either (lefts, rights)
import Control.Applicative (Alternative, (<|>), many, some, empty)
import Grammar ( Expr (Number, Boolean, Sum, Minus, If, Var, Let, Compare, Assign, Block))
-- import Debug.Trace
trace s o = o

class Default d where
    def :: d

newtype Parser a = Parser {runParser :: Meta String -> Either String (String, Meta a) }

data Meta o = MetaData { out :: o, column :: Int, line :: Int, err :: String, raise :: Bool}


raiseError :: String -> Meta o -> Meta o
raiseError s (MetaData o c l e r) = MetaData o c l s True

incColumn:: Meta o -> Meta o
incColumn (MetaData o c l e r) = MetaData o (c + 1) l e r

resColumn:: Meta o -> Meta o
resColumn (MetaData o _ l e r) = MetaData o 0 l e r

incLine:: Meta o -> Meta o
incLine (MetaData o c l e r) = MetaData o c (l + 1) e r

metaC :: o -> Meta o
metaC c =  MetaData c 0 0 "" False

metaTest :: o -> Meta o
metaTest c =  MetaData c 0 10 "" False


switch :: a -> Meta o -> Meta a
switch s (MetaData o c l e r) = MetaData s c l e r

instance Control.Monad.Functor Meta where
  fmap :: (a -> b) -> Meta a -> Meta b
  fmap f m = MetaData (f $ out m) (column m) (line m) (err m) (raise m)

instance Control.Monad.Applicative Meta where
    pure :: a -> Meta a
    pure = metaTest
    
    MetaData f _ _ _ _ <*> MetaData m c l e True = MetaData (f m) (trace "test" c) l e True
    MetaData f cf lf ef True <*> MetaData m _ _ _ _ = MetaData (f m) (trace "test" cf) lf ef True    
    MetaData f _ _ _ _ <*> MetaData m c l e r = MetaData (f m) (trace "test" c) l e r
    

instance Default o => Default (Meta o) where
    def = MetaData def 0 0 "" False

instance Show (Meta o) where
--   show (MetaData o c l e True) = "Error : " ++ e
  show (MetaData o c l _ _) = "column : " ++ show c ++  " - line : " ++ show l 


instance Control.Monad.Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser $ \d -> do
        (s', o) <- runParser p d
        return (s', f <$> o)

instance Control.Monad.Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \s -> Right ("", pure x)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    Parser f <*> Parser a = Parser $ \d -> do
            (s, d') <- f d
            (s', d'') <- a (switch s (trace ("<*> " ++ show d) d'))
            Right (s', out d' <$> d'')

instance Control.Monad.Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    Parser p >>= f =  Parser $ \d -> do
            (s, d') <- trace "running p " p d
            (s', d'') <- runParser (trace "Running f" f $ out d') $ switch s (trace (" >>= ms" ++ show d') d')
            Right (s', trace ("GG" ++ show d'') d'')

instance Control.Monad.Alternative Parser where
    empty :: Parser a
    empty = Parser $ \s -> do
            Left ""
    (<|>) :: Parser a -> Parser a -> Parser a
    Parser a <|> Parser b = Parser $ \s -> do
            case a s of
                Right (o, s') -> Right (o, s')
                Left _ -> b s

char' :: Char -> Parser Char
char' c = Parser charP where
    charP d = case out d of
        [] -> Left ""
        (x:xs) -> fn x where
            fn ch | c == x = Right (trace "c == x" xs, switch c d )
                  | otherwise = Left ""

metaUpdater :: Char -> Parser Char
metaUpdater c = Parser updater where
        updater ms | c == '\n' = Right (out ms, ( resColumn . incLine) $ switch c (trace (" updater ms " ++ show ms) ms))
                   | otherwise = Right (trace "bob" out ms, incColumn $ switch c (trace ("updater ms " ++ show ms) ms))

char :: Char -> Parser Char
char c = char' c >>= metaUpdater

string :: String -> Parser String
string = mapM char



action s = do 
    m <- string "Test"
    return "ez"

            
    