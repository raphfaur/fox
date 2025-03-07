module Frontend where

import Control.Monad
import qualified Control.Monad as Control
import qualified Control.Applicative as Control.Monad
import Control.Applicative (Alternative, (<|>), many, some)
import Data.Char (ord)

import Grammar ( Expr (Number, Boolean, Sum, Minus, If, Var, Let, Compare, Assign, Block))
import Parser
import Debug.Trace (trace)

char :: Char -> Parser Char
char c = Parser charP where
    charP [] = Nothing
    charP (x:xs) | x == c = Just (c, xs)
                 | otherwise = Nothing

string :: String -> Parser String
string = mapM char

ss :: Parser String
ss = many $ (char ' ' <|> char '\n')

lock:: Char -> Parser Char
lock c = Parser charLock where
    charLock [] = Nothing
    charLock (x:xs) | x == c = Nothing
                    | x == '(' = Nothing
                    | x == '\n' = Nothing
                    | x == ';' = Nothing
                    | otherwise = Just (x, xs)

until' :: Char -> Parser String
until' c = many $ lock c

-----------
-- NUMERIC
-----------

numChar = char '1' <|> char '2' <|> char '3' <|>
          char '4' <|> char '5' <|> char '6' <|>
          char '7' <|> char '8' <|> char '9' <|>
          char '0'

intChar :: Parser String
intChar = some numChar

str2int :: String -> Int
str2int = foldl mul 0 where
    mul a b = 10 * a + (ord b - ord '0')

int :: Parser Int
int = str2int <$> intChar

parseLitterals = (Number <$> int) <|> 
    (string "True" *> pure (Boolean True)) <|>
    (string "False" *> pure (Boolean False))

parseIf = do
    string "if" *> ss
    cond <- parseExpr <* ss
    char ':' <* ss
    then_instr <- parseBlock <* ss
    string "else" <* ss  <* char ':' <* ss
    else_instr <- parseBlock <*ss
    return $ If cond then_instr else_instr

remaining :: Parser String
remaining = Parser $ \s -> Just (s, s)

nameChecker :: String -> Parser String
nameChecker [] =  Parser $ const Nothing
nameChecker (x:xs) | x == '(' = Parser $ const Nothing
                   | x == ')' = Parser $ const Nothing
                   | otherwise = return (x:xs)


varName = until' ' ' >>= nameChecker
parseVar = Var <$> varName

parseLet = do
    string "let" <* ss
    id <- until' ' ' <* ss
    char '=' <* ss
    value <- parseExpr
    return $ Let id value

parseExpr = parseBinOp <|> parseAssign <|> parseLitterals <|> parseIf <|> parseLet <|> parseVar

parseExpr' =  parseLitterals <|> parseIf <|> parseLet <|> parseVar

parseBinOp = do
    a <- parseTerm
    ss
    op <- char '+' *> pure Sum <|> char '-' *> pure Minus <|> string "==" *> pure Compare
    ss
    b <- parseTerm
    return $ op a b

parseTerm = (do 
    char '('
    ss
    e <- parseExpr
    ss
    char ')'
    ss
    return e) <|> parseExpr' 

parseAssign = do 
    v <- varName
    ss
    char '='
    ss
    e <- parseExpr
    ss
    return $ Assign v e

parseBlock = do
    ex <- some (parseExpr <* ss <* char ';' <* ss )
    return $ Block ex

parseAll s = case runParser parseBlock s of 
    (Just (e, v)) -> e
    