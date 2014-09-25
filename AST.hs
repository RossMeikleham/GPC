{- GPC abstract syntax tree -}

module AST(
      Program(..)
    , TopLevel(..)
    , Stmt(..)
    , Expr(..)
    , BinOps(..)
    , UnaryOps(..)
    , Number(..)
    ) where

data Program = Program [TopLevel]


-- |Top Level Expressions
data TopLevel =
        Func String String [(String, String)] [Stmt]  -- Return Type, Name, Arguments, Code
      | GlobalDecl Expr 


-- |Statement
data Stmt = 
        Decl Expr
      | Expr  
    
-- |Expression
data Expr =
      BinOp BinOps Expr Expr
    | UnaryOp UnaryOps Expr
    | FunCall Expr


-- |Binary Operators
data BinOps =
      Add 
    | Sub
    | Mul
    | Div
    | And
    | Or
    | Equals
    | NEquals
    | ShiftL
    | ShiftR
    | BAnd
    | BXor
    | BOr

-- |Unary Operators
data UnaryOps =
      Not
    | Neg
    | BNot 

-- |Number Types
data Number =
      Int
    | Double
    
