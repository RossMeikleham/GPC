{- GPC abstract syntax tree -}

module AST where

data Program = Program [TopLevel]


-- |Top Level Expressions
data TopLevel
      | Func String String [(String, String)] [Stmt]  -- Return Type, Name, Arguments, Code
      | GlobalDecl Expr 


-- |Statement
data Stmt = 
        Decl Expr
      | Expr  
    
-- |Expression
data Expr =
    BinOp BinOps Expr Expr
    UnaryOp UnaryOps Expr
    FunCall Expr


-- |Binary Operators
data BinOps =
      Add 
    | Sub
    | Mul
    | Div
    | And
    | Or

-- |Unary Operators
data UnaryOps =
      Not
    | Xor

-- |Number Types
data Number =
      Int
    | Double
    
