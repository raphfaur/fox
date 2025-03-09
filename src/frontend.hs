module Frontend where

import Control.Applicative (Alternative, many, some, (<|>))
import Control.Applicative qualified as Control.Monad
import Control.Monad
import Control.Monad qualified as Control
import Data.Char (ord)
import Grammar (Expr (Assign, Block, Boolean, Call, Compare, Func, If, Let, Minus, Number, Return, Sum, Var, CallBlock, Print))
import Parser

char :: Char -> Parser Char
char c = Parser charP
  where
    charP [] = Nothing
    charP (x : xs)
      | x == c = Just (c, xs)
      | otherwise = Nothing

string :: String -> Parser String
string = mapM char

ss :: Parser String
ss = many $ (char ' ' <|> char '\n')

lock :: Char -> Parser Char
lock c = Parser charLock
  where
    charLock [] = Nothing
    charLock (x : xs)
      | x == c = Nothing
      | otherwise = Just (x, xs)

until' :: Char -> Parser String
until' c = many $ lock c

-----------
-- NUMERIC
-----------

numChar =
  char '1'
    <|> char '2'
    <|> char '3'
    <|> char '4'
    <|> char '5'
    <|> char '6'
    <|> char '7'
    <|> char '8'
    <|> char '9'
    <|> char '0'

intChar :: Parser String
intChar = some numChar

str2int :: String -> Int
str2int = foldl mul 0
  where
    mul a b = 10 * a + (ord b - ord '0')

int :: Parser Int
int = str2int <$> intChar

parseLitterals =
  (Number <$> int)
    <|> (string "True" *> pure (Boolean True))
    <|> (string "False" *> pure (Boolean False))

parseIf = do
  string "if" *> ss
  char '('
  ss
  cond <- parseExpr <* ss
  char ')'
  ss
  then_instr <- parseBlock <* ss
  string "else" <* ss <* ss
  else_instr <- parseBlock <* ss
  return $ If cond then_instr else_instr

remaining :: Parser String
remaining = Parser $ \s -> Just (s, s)

nameChecker :: String -> Maybe String
nameChecker [] = Nothing
nameChecker "let" = Just "Forbidden"
nameChecker "if" = Just "Forbidden"
nameChecker "else" = Just "Forbidden"
nameChecker "return" = Just "Forbidden"
nameChecker "func" = Just "Forbidden"
nameChecker "print" = Just "Forbidden"
nameChecker (x : xs)
  | x == '(' = Just $ show x ++ "not allowed in variable name"
  | x == ')' = Just $ show x ++ "not allowed in variable name"
  | x == '{' = Just $ show x ++ "not allowed in variable name"
  | x == '}' = Just $ show x ++ "not allowed in variable name"
  | x == ',' = Just $ show x ++ "not allowed in variable name"
  | x == '+' = Just $ show x ++ "not allowed in variable name"
  | otherwise = nameChecker xs

var = Parser $ \s ->
  let v = runParser (until' ' ') s
   in case v of
        Just (o, s') -> case nameChecker o of
          Nothing -> Just (o, s')
          Just _ -> Nothing
        Nothing -> Nothing

varName = var

parseVar = Var <$> varName

parseLet = do
  string "let" <* ss
  id <- varName <* ss
  char '=' <* ss
  value <- parseExpr <* ss
  char ';'
  return $ Let id value

parseExpr =  parsePrint <|> parseReturn <|> parseCall <|> parseBinOp <|> parseAssign <|> parseLitterals <|> parseIf <|> parseLet <|> parseVar <|> parseFunc <|> parseBlock

parseExpr' = parseCall <|> parseLitterals <|> parseIf <|> parseLet  <|> parseVar <|> parsePrint

parseBinOp = do
  a <- parseTerm
  ss
  op <- char '+' *> pure Sum <|> char '-' *> pure Minus <|> string "==" *> pure Compare
  ss
  b <- parseTerm
  ss
  return $ op a b

parseTerm =
  ( do
      char '('
      ss
      e <- parseExpr
      ss
      char ')'
      ss
      return e
  )
    <|> ( do
      ss
      e <- parseExpr'
      ss
      return e
      )

parseAssign = do
  v <- varName
  ss
  char '='
  ss
  e <- parseExpr
  ss
  char ';'
  return $ Assign v e

parseArg = do
  ss
  name <- varName
  ss
  char ','
  ss
  return name

parseEmptyArgs :: Parser [String]
parseEmptyArgs = do
  char '('
  ss
  char ')'
  return []

parseArgs = do
  ss
  char '('
  ss
  args <- many parseArg
  ss
  char ')'
  ss
  return args

parseFunc = do
  string "func"
  ss
  name <- varName
  ss
  args <- parseEmptyArgs <|> parseArgs
  ss
  block <- parseCallBlock
  ss
  return $ Func name args block

parseCall = do
  ss
  n <- varName
  ss
  char '('
  ss
  e <- many parseCallArgs
  ss
  char ')'
  ss
  return $ Call n e

parseCallArgs = do
  e <- parseExpr
  ss
  char ','
  ss
  return e

parseBlock = do
  char '{'
  ss
  ex <- many (parseExpr <* ss)
  ss
  char '}'
  return $ Block ex

parseCallBlock = do
  char '{'
  ss
  ex <- many (parseExpr <* ss)
  ss
  char '}'
  return $ CallBlock ex

parseReturn = do
  string "return"
  ss
  e <- parseExpr
  ss
  char ';'
  ss
  return $ Return e

parsePrint = do 
  string "print"
  ss
  Print <$> parseExpr

parseAll s = case runParser parseBlock ("{" ++ s ++ "}") of
  (Just (e, v)) -> e