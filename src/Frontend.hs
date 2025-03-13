{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Frontend where

import Control.Applicative (many, some, (<|>))
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
ss = many (char ' ' <|> char '\n')

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

numChar :: Parser Char
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

parseLitterals :: Parser Expr
parseLitterals =
  (Number <$> int)
    <|> (string "True" *> pure (Boolean True))
    <|> (string "False" *> pure (Boolean False))

parseIf :: Parser Expr
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
  | x == '\n' = Just $ show x ++ "not allowed in variable name"
  | otherwise = nameChecker xs

var :: Parser String
var = Parser $ \s ->
  let v = runParser (until' ' ') s
   in case v of
        Just (o, s') -> case nameChecker o of
          Nothing -> Just (o, s')
          Just _ -> Nothing
        Nothing -> Nothing

varName :: Parser String
varName = var

parseVar :: Parser Expr
parseVar = Var <$> var

parseLet :: Parser Expr
parseLet = do
  string "let" <* ss
  id <- var <* ss
  char '=' <* ss
  value <- parseExpr <* ss
  char ';'
  return $ Let id value

parseExpr :: Parser Expr
parseExpr =  parsePrint <|> parseReturn <|> parseCall <|> parseBinOp <|> parseAssign <|> parseLitterals <|> parseIf <|> parseLet <|> parseVar <|> parseFunc <|> parseBlock

parseExpr' :: Parser Expr
parseExpr' = parseCall <|> parseLitterals <|> parseIf <|> parseLet  <|> parseVar <|> parsePrint

parseBinOp :: Parser Expr
parseBinOp = do
  a <- parseTerm
  ss
  op <- char '+' *> pure Sum <|> char '-' *> pure Minus <|> string "==" *> pure Compare
  ss
  b <- parseTerm
  ss
  return $ op a b

parseTerm :: Parser Expr
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

parseAssign :: Parser Expr
parseAssign = do
  v <- var
  ss
  char '='
  ss
  e <- parseExpr
  ss
  char ';'
  return $ Assign v e

parseArg :: Parser String
parseArg = do
  ss
  name <- var
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

parseArgs :: Parser [String]
parseArgs = do
  ss
  char '('
  ss
  args <- many parseArg
  ss
  char ')'
  ss
  return args

parseFunc :: Parser Expr
parseFunc = do
  string "func"
  ss
  name <- var
  ss
  args <- parseEmptyArgs <|> parseArgs
  ss
  block <- parseCallBlock
  ss
  return $ Func name args block

parseCall :: Parser Expr
parseCall = do
  ss
  n <- var
  ss
  char '('
  ss
  e <- many parseCallArgs
  ss
  char ')'
  ss
  return $ Call n e

parseCallArgs :: Parser Expr
parseCallArgs = do
  e <- parseExpr
  ss
  char ','
  ss
  return e

parseBlock :: Parser Expr
parseBlock = do
  char '{'
  ss
  ex <- many (parseExpr <* ss)
  ss
  char '}'
  return $ Block ex

parseCallBlock :: Parser Expr
parseCallBlock = do
  char '{'
  ss
  ex <- many (parseExpr <* ss)
  ss
  char '}'
  return $ CallBlock ex

parseReturn :: Parser Expr
parseReturn = do
  string "return"
  ss
  e <- parseExpr
  ss
  char ';'
  ss
  return $ Return e

parsePrint :: Parser Expr
parsePrint = do
  string "print"
  ss
  Print <$> parseExpr

parseAll :: [Char] -> Expr
parseAll s = case runParser parseBlock ("{" ++ s ++ "}") of
  (Just (e, _)) -> e
