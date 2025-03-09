module Grammar (Expr (Number, Boolean, Sum, Minus, If, Var, Let, Compare, Assign, Block, CallBlock, Func, Call, Return)) where

type Ident = String

data Expr
  = Number Int
  | Boolean Bool
  | Sum Expr Expr
  | Minus Expr Expr
  | If Expr Expr Expr
  | Var Ident
  | Let Ident Expr
  | Compare Expr Expr
  | Assign Ident Expr
  | Block [Expr]
  | CallBlock [Expr]
  | Func Ident [Ident] Expr
  | Call Ident [Expr]
  | Return Expr
  deriving (Show)