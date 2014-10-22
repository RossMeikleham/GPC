{- GPC abstract syntax tree -}

module GPC.AST(    
      Program(..)
    , TopLevel(..)
    , Stmt(..)
    , Expr(..)
    , BinOps(..)
    , UnaryOps(..)
    , Literal(..)
    ) where

data Program = Program [TopLevel] deriving Show


-- |Top Level Expressions
data TopLevel =
        Func Type Ident [(Type, Ident)] [Stmt]  -- |Return Type, Name, Arguments, Code
      | TLObjs Objects -- |External objects
      | TlAssign ConstAssignment 
       deriving Show


data Objects = Obj1 ClassName -- |Single Object of Class
             | ObjM ClassName ConstExpr -- |Array of Objects
               

-- |Statement
data Stmt = 
        Assign Type Ident Expr -- |Type Name, assignment
      | Seq BlockStmt -- |Evaluate statements in sequential order
      | BStmt BlockStmt -- | Statements in enclosed block
      | Exp Expr
      | If Expr Stmt  
      | IfElse Expr Stmt Stmt
      | Return Expr
      | For Int Int Int Stmt --Start, Stop, Step, statements
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

data ConstAssignment = ConstAssignment Ident Ident ConstExpr

-- |Constant Expressions which can be evaluated at compile-time
data ConstExpr =
     ConstBinOp BinOps ConstExpr ConstExpr 
    |ConstUnaryOp UnaryOps ConstExpr
    |ConstLit Literal


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


data ClassName = ClassName String
data Ident = Ident String
data Type = Ident String
data BlockStmt = [Stmt]
