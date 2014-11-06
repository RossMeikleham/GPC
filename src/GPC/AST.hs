{- GPC abstract syntax tree -}

module GPC.AST(    
      Program(..)
    , TopLevel(..)
    , Stmt(..)
    , Expr(..)
    , BinOps(..)
    , UnaryOps(..)
    , Literal(..)
    , Ident(..)
    , Type(..)
    , BlockStmt(..)
    , Assign(..)
    , FunCall(..)
    ) where

data Program = Program [TopLevel] deriving Show


-- |Top Level Expressions
data TopLevel =
        Func Type Ident [(Type, Ident)] BlockStmt  -- |Return Type, Name, Arguments, Code
      | TLObjs Objects -- |External objects
      | TLAssign Assign
       deriving Show


data Objects = Obj1 ClassName -- |Single Object of Class
             | ObjM ClassName Expr -- |Array of Objects
              deriving Show               

-- |Statement
data Stmt = 
        AssignStmt Assign -- |Type Name, assignment
      | Seq BlockStmt -- |Evaluate statements in sequential order
      | BStmt BlockStmt -- | Statements in enclosed block
      | FunCallStmt FunCall
      | Exp Expr
      | If Expr Stmt  
      | IfElse Expr Stmt Stmt
      | Return Expr
      | ForLoop Expr Expr Expr Stmt --Start, Stop, Step, statements
      | None -- Blank statement
       deriving Show

data Assign = Assign Type Ident Expr deriving Show
data FunCall = FunCall Ident [Expr] deriving Show

-- |Expression
data Expr =
      ExpBinOp BinOps Expr Expr
    | ExpUnaryOp UnaryOps Expr
    | ExpFunCall FunCall
    | ExpIdent Ident
    | ExpLit Literal
     deriving Show


-- |Binary Operators
data BinOps =
      Add 
    | Sub
    | Mul
    | Div
    | And
    | Or
    | Mod
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
    | Number (Either Integer Double)
    | Bl Bool
     deriving Show


data ClassName = ClassName String deriving Show
data Ident = Ident String deriving (Show, Eq, Ord)
data Type = Type String deriving (Show, Eq)
data BlockStmt = BlockStmt [Stmt] deriving Show
