{- GPC abstract syntax tree -}

module GPC.AST(    
      Program(..)
    , TopLevel(..)
    , Stmt(..)
    , Expr(..)
    , BinOps(..)
    , UnaryOps(..)
    , Literal(..)
    , FunStmt(..)
    ) where

data Program = Program [TopLevel] deriving Show


-- |Top Level Expressions
data TopLevel =
        Func String String [(String, String)] [FunStmt]  -- Return Type, Name, Arguments, Code
      | TlStmt Stmt
       deriving Show

-- | Function statements, can include return
data FunStmt =
       FStmt Stmt
     | Return Expr
      deriving Show

-- |Statement
data Stmt = 
        Decl String String Expr --Type Name, assignment
      | Seq [Stmt] -- |Evauldate statements in sequential order
      | Par [Stmt] -- |Evaluate statement in parallel (default)
      | Exp Expr  
      | None -- Blank statement
       deriving Show

-- |Expression
data Expr =
      BinOp BinOps Expr Expr
    | UnaryOp UnaryOps Expr
    | FunCall String [Expr] --Name args
    | Ident String
    | Lit Literal
     deriving Show

-- |Binary Operators
data BinOps =
      Add 
    | Sub
    | Mul
    | Div
    | And
    | Or
    | Less
    | LessEq
    | Greater
    | GreaterEq
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


data Literal =
      Str String
    | Ch Char
    | Num (Either Integer Double)
    | Bl Bool
     deriving Show



