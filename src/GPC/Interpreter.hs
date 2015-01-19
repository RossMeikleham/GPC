{-# LANGUAGE GADTs, KindSignatures #-} --GADTSyntax #-}
 

module Interpreter where

import Control.Monad.State.Lazy
import qualified Data.Map as M

type ConstTable a = M.Map String a
--type ConstVarTable = M.Map (Ident SrcPos) (Literal SrcPos)
--type FunTable = M.Map (Ident SrcPos) (Type SrcPos, [Type SrcPos])
--type ObjectTable = M.Map (Ident SrcPos) (Objects SrcPos)


data Stmt :: * -> * where
    
    StmtGetVar :: Ident -> Stmt Value
    StmtSetVar :: Ident -> Value -> Stmt () -- ^ assignment
    
    Seq :: [Stmt a] -> Stmt () -- ^ Evaluate statements in sequential order
    BStmt :: [Stmt a] -> Stmt () -- ^ Statements in enclosed block
    FunCallStmt :: Ident -> [Value] -> [Stmt a] -> Stmt ()
    If :: Expr Bool -> Stmt () -> Stmt () 
    IfElse :: Expr Bool -> Stmt () -> Stmt () -> Stmt ()
    GReturn :: Expr a -> Stmt () -- ^ Return value from current function
    For :: Expr a -> Expr Bool -> Expr b -> [Stmt c] -> Stmt () 

    StmtReturn :: a -> Stmt a
    StmtBind :: Stmt a -> Stmt Bool -> Stmt b -> Stmt c -> Stmt ()    

data Expr :: * -> * where
    
    GetVar :: Ident -> Expr Value
    SetVar :: Ident -> Value -> Expr ()
    
  --  ExpFunCall ::  (FunCall a)   -- ^ Function Call
  --  ExpIdent :: Ident  (Ident a)           -- ^ Identifier  
  --  ExpLit (Literal a)           -- ^ Constant/Literal value
  --  ExpPointer (Pointer a)       -- ^ Pointer value

    -- ^ Boolean Operations
    Add  ::   Num a  => Expr a -> Expr a -> Expr a
    Mul  ::   Num a  => Expr a -> Expr a -> Expr a
    Sub  ::   Num a  => Expr a -> Expr a -> Expr a
    Div  ::   Num a  => Expr a -> Expr a -> Expr a
    Eqls ::   Eq a   => Expr a -> Expr a -> Expr Bool
    NEq  ::   Eq a   => Expr a -> Expr a -> Expr Bool
    Less ::   Ord a  => Expr a -> Expr a -> Expr Bool
    LessEq :: Ord a  => Expr a -> Expr a -> Expr Bool
    Gtr ::    Ord a  => Expr a -> Expr a -> Expr Bool
    GtrEq ::  Ord a  => Expr a -> Expr a -> Expr Bool
    And :: Expr Bool -> Expr Bool -> Expr Bool
    Or  :: Expr Bool -> Expr Bool -> Expr Bool
    Mod ::    Expr Int -> Expr Int -> Expr Int
    ShiftL :: Expr Int -> Expr Int -> Expr Int
    ShiftR :: Expr Int -> Expr Int -> Expr Int
    BAnd ::   Expr Int -> Expr Int -> Expr Int
    BOr  ::   Expr Int -> Expr Int -> Expr Int
    BXor ::   Expr Int -> Expr Int -> Expr Int
   
    -- ^ Unary Operations  
    Not  :: Expr Bool -> Expr Bool    -- ^ Boolean NOT
    Neg  :: Num a => Expr a -> Expr a -- ^ Number negation
    BNot :: Expr Int -> Expr Int      -- ^ Bitwise NOT

    
    ExprReturn :: a -> Expr a
    ExprBind :: Expr a -> Expr Bool -> Expr b -> Expr c -> Expr ()    

data Value = Var Ident | GNum Int
data Ident = Ident String






