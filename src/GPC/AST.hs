{- GPC abstract syntax tree -}

module GPC.AST(    
      Program(..)
    , TopLevel(..)
    , Stmt(..)
    , Expr(..)
    , BinOps(..)
    , UnaryOps(..)
    , Number(..)
    ) where

data Program = Program [TopLevel] deriving Show


-- |Top Level Expressions
data TopLevel =
        Func String String [(String, String)] [Stmt]  -- Return Type, Name, Arguments, Code
      | TlStmt Stmt
       deriving Show


-- |Statement
data Stmt = 
        Decl Expr
      | Exp Expr  
       deriving Show

-- |Expression
data Expr =
      BinOp BinOps Expr Expr
    | UnaryOp UnaryOps Expr
    | FunCall Expr
    | None
     deriving Show

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
     deriving Show


-- |Unary Operators
data UnaryOps =
      Not
    | Neg
    | BNot 
     deriving Show


-- |Number Types
data Number =
      Int
    | Double
     deriving Show
