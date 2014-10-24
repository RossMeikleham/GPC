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
    , ConstExpr(..)
    , ConstAssign(..)
    ) where

data Program = Program [TopLevel] deriving Show


-- |Top Level Expressions
data TopLevel =
        Func Type Ident [(Type, Ident)] BlockStmt  -- |Return Type, Name, Arguments, Code
      | TLObjs Objects -- |External objects
      | TlAssign ConstAssign
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
      | For Int Int Int Stmt --Start, Stop, Step, statements
      | None -- Blank statement
       deriving Show

data ConstAssign = ConstAssign Type Ident ConstExpr deriving Show
data Assign = Assign Type Ident Expr deriving Show
data FunCall = FunCall String [Expr] deriving Show

-- |Expression
data Expr =
      ExpBinOp BinOps Expr Expr
    | ExpUnaryOp UnaryOps Expr
    | ExpFunCall FunCall
    | ExpIdent Ident
    | ExpLit Literal
     deriving Show

-- |Constant Expression, cna be determined at compile time
data ConstExpr =
      ConstBinOp BinOps ConstExpr ConstExpr
    | ConstUnaryOp UnaryOps ConstExpr
    | ConstIdent Ident
    | ConstLit Literal
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


data ClassName = ClassName String deriving Show
data Ident = Ident String deriving Show
data Type = Type String deriving Show
data BlockStmt = BlockStmt [Stmt] deriving Show
