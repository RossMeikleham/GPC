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
    , ClassName(..)
    , Objects(..)
    , MethodCall(..)
    , Var(..)
    , ConstructObjs(..)
    , LibName(..)
    , ClassName(..)
    ) where

data Program = Program [TopLevel] deriving Show


-- | Top Level Expressions
data TopLevel =
        Func Type Ident [(Type, Ident)] BlockStmt  -- ^ Return Type, Name, Arguments, Code
      | TLObjs Objects -- ^ External objects
      | TLConstructObjs ConstructObjs -- ^ External object constructor calls
      | TLAssign Assign
       deriving Show

-- | Objects
data Objects = Objects {
   objLName :: LibName,
   objCName :: ClassName,
   objVar ::Var 
} deriving Show

-- | Variable
data Var = 
          VarArrayElem Ident Expr -- ^ Element of Array
        | VarIdent Ident -- ^ Ordinary identifier
         deriving Show


data ConstructObjs = ConstructObjs Var LibName ClassName [Expr] deriving Show

-- | Statement
data Stmt = 
        AssignStmt Assign -- ^ Type Name, assignment
      | Seq BlockStmt -- ^ Evaluate statements in sequential order
      | BStmt BlockStmt -- ^ Statements in enclosed block
      | FunCallStmt FunCall -- ^ Call Funcion
      | MethodStmt MethodCall -- ^ Call Object method
      | If Expr Stmt  -- ^ If statement
      | IfElse Expr Stmt Stmt -- ^ If Else statement
      | Return Expr -- ^ Return value from current function
      | ForLoop Ident Expr Expr Expr BlockStmt -- ^ Start, Stop, Step, statements, static for loops
       deriving Show

data Assign = Assign Type Ident Expr deriving Show -- ^ Variable assignment
data FunCall = FunCall Ident [Expr] deriving Show -- ^ Function call layout
data MethodCall = MethodCall Ident Ident [Expr] deriving Show -- ^ Method call layout

-- | Expression
data Expr =
      ExpBinOp BinOps Expr Expr
    | ExpUnaryOp UnaryOps Expr
    | ExpFunCall FunCall
    | ExpMethodCall MethodCall
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
     deriving (Eq)

instance Show Literal where
    show (Str s) = s
    show (Ch c) = show c
    show (Number (Left i)) = show i
    show (Number (Right d)) = show d
    show (Bl b) = show b


type LibName = Ident 
type ClassName = Ident 

data Ident = Ident String deriving (Eq, Ord)
instance Show Ident where
    show (Ident s) = s

data Type = Type String deriving (Show, Eq)
data BlockStmt = BlockStmt [Stmt] deriving Show
