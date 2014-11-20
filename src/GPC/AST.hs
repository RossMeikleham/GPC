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

-- | Statement
-- TODO Add method calls from objects, are completely seperarate
-- from local function calls, as these call the Kernel C++ objects
data Stmt = 
        AssignStmt Assign -- ^ Type Name, assignment
      | Seq BlockStmt -- ^ Evaluate statements in sequential order
      | BStmt BlockStmt -- ^ Statements in enclosed block
      | FunCallStmt FunCall -- ^ Call Funcion
      | If Expr Stmt  -- ^ If statement
      | IfElse Expr Stmt Stmt -- ^ If Else statement
      | Return Expr -- ^ Return value from current function
      | ForLoop Ident Expr Expr Expr BlockStmt --Start, Stop, Step, statements, static for loops
       deriving Show

data Assign = Assign Type Ident Expr deriving Show -- ^ Variable assignment
data FunCall = FunCall Ident [Expr] deriving Show -- ^ Function call layout

-- | Expression
data Expr =
      ExpBinOp BinOps Expr Expr
    | ExpUnaryOp UnaryOps Expr
    | ExpFunCall FunCall
    | ExpIdent Ident
    | ExpLit Literal
     deriving Show


-- | Binary Operators
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
     deriving (Show, Eq)


-- | Unary Operators
data UnaryOps =
      Not
    | Neg
    | BNot 
     deriving (Show, Eq)

-- | Literal/Constants
data Literal =
      Str String
    | Ch Char
    | Number (Either Integer Double)
    | Bl Bool
     deriving (Show, Eq)


data ClassName = ClassName String deriving Show
data Ident = Ident String deriving (Show, Eq, Ord)
data Type = Type String deriving (Show, Eq)
data BlockStmt = BlockStmt [Stmt] deriving Show
