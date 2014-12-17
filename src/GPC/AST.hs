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
    , ClassName
    , Objects(..)
    , MethodCall(..)
    , Var(..)
    , ConstructObjs(..)
    , LibName
    , Pointer(..)
    ) where

data Program = Program [TopLevel] deriving Show


-- | Top Level Expressions
data TopLevel =
        Func Type Ident [(Type, Ident)] BlockStmt  -- ^ Return Type, Name, Arguments, Code
      | TLObjs Objects -- ^ External objects
      | TLConstructObjs ConstructObjs -- ^ External object constructor calls
      | TLAssign Assign -- ^ Top level assignment
       deriving Show

-- | Objects
data Objects = Objects {
   nameSpace :: [Ident],
   objVar ::Var 
} deriving Show

-- | Variable
data Var = 
          VarArrayElem Ident Expr -- ^ Element of Array
        | VarIdent Ident -- ^ Ordinary identifier
         deriving Show

-- | Constructing Objects
data ConstructObjs = ConstructObjs [Ident]  Var [Expr] deriving Show

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
      ExpBinOp BinOps Expr Expr -- ^ Binary operation with 2 sub-expressions
    | ExpUnaryOp UnaryOps Expr -- ^ Unary operation with sub-expression
    | ExpFunCall FunCall -- ^ Function Call
    | ExpMethodCall MethodCall -- ^ C++ Object method call
    | ExpIdent Ident -- ^ Identifier  
    | ExpLit Literal -- ^ Constant/Literal value
    | ExpPointer Pointer -- ^ Pointer value
     deriving Show


-- | Binary Operators
data BinOps =
      Add -- ^ Addition
    | Sub -- ^ Subtraction
    | Mul -- ^ Multiplication
    | Div -- ^ Division
    | And -- ^ Boolean AND
    | Or  -- ^ Boolean OR
    | Mod -- ^ Modulo
    | Less -- ^ Less than 
    | LessEq -- ^ Less than or equal to
    | Greater -- ^ Greater than
    | GreaterEq -- ^ Greather than or equal to
    | Equals -- ^ Equals
    | NEquals -- ^ Not Equal
    | ShiftL -- ^ Logical Shift Left
    | ShiftR -- ^ Logical Shift Right
    | BAnd -- ^ Bitwise AND
    | BXor -- ^ Bitwise XOR
    | BOr -- ^ Bitwise OR
     deriving (Show, Eq)


-- | Unary Operators
data UnaryOps =
      Not -- ^ Boolean NOT
    | Neg -- ^ Number negation
    | BNot -- ^ Bitwise NOT
     deriving (Show, Eq)

-- | Literal/Constants
data Literal =
      Str String -- ^ String
    | Ch Char -- ^ Char
    | Number (Either Integer Double) -- ^ Numbers, either Int/Double
    | Bl Bool -- Boolean
     deriving (Eq)

instance Show Literal where
    show (Str s) = s
    show (Ch c) = show c
    show (Number (Left i)) = show i
    show (Number (Right d)) = show d
    show (Bl b) = show b


-- | C++ Library
type LibName = Ident

-- | C++ Class Name  
type ClassName = Ident 

-- | Identifier
data Ident = Ident String deriving (Eq, Ord)
instance Show Ident where
    show (Ident s) = s

data Pointer = Pointer Ident Integer deriving (Show, Eq) -- |Pointer to array elem with offset

-- | Types
data Type = PointerType Type -- Pointer to a Type
          | NormalType { 
              isKernel :: Bool, -- ^ is base type in Kernel
              typeName :: String -- ^ type name (e.g. int, bool)
            }
          deriving (Show, Eq) 

-- | Block of Statements
data BlockStmt = BlockStmt [Stmt] deriving Show
