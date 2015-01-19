{-# LANGUAGE GADTs, KindSignatures #-} 
 
{- Interpreter which runs through the GPC code
 - and produces the appropriate td Kernel Calls to the GPRM -}
module Interpreter (runProgram) where

--TODO temp, replace with GPIR AST Symbol Tree

import           Control.Monad.State.Lazy
import qualified Data.Map as M

type ConstTable a = M.Map String a
data SymbolTree = SymbolTree String 

--type ConstVarTable = M.Map (Ident SrcPos) (Literal SrcPos)
--type FunTable = M.Map (Ident SrcPos) (Type SrcPos, [Type SrcPos])
--type ObjectTable = M.Map (Ident SrcPos) (Objects SrcPos)

data TLStmt :: * -> * where
    FuncDef   :: Ident -> [Stmt a] -> TLStmt () 
    TLAssign  :: Expr a -> TLStmt ()
       
    TLStmtReturn :: a -> TLStmt a
    TLStmtBind :: TLStmt a -> (a -> TLStmt b) -> TLStmt b    

instance Monad TLStmt where
    return = TLStmtReturn
    (>>=)  = TLStmtBind


data Stmt :: * -> * where
    
    StmtGetVar :: Ident -> Stmt Value
    StmtSetVar :: Ident -> Value -> Stmt () -- ^ Assignment
    
    Seq :: [Stmt a] -> Stmt () -- ^ Evaluate statements in sequential order
    BStmt :: [Stmt a] -> Stmt () -- ^ Statements in enclosed block
    FunCallStmt :: Ident -> [Value] -> [Stmt a] -> Stmt ()
    If :: Expr Bool -> Stmt () -> Stmt () 
    IfElse :: Expr Bool -> Stmt () -> Stmt () -> Stmt ()
    GReturn :: Expr a -> Stmt () -- ^ Return value from current function
    For :: Expr a -> Expr Bool -> Expr b -> [Stmt c] -> Stmt () 

    StmtReturn :: a -> Stmt a
    StmtBind :: Stmt a -> (a -> Stmt b) -> Stmt b    

instance Monad Stmt where
    return = StmtReturn
    (>>=)  = StmtBind

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

    -- ^ Monad implementation
    ExprReturn :: a -> Expr a
    ExprBind :: Expr a -> (a -> Expr b) -> Expr b     

instance Monad Expr where
    return = ExprReturn
    (>>=)  = ExprBind


data Value = Var Ident | GNum Int
data Ident = Ident String
type Vars = M.Map Ident Value



runProgram :: [TLStmt a] -> State Vars [SymbolTree]
runProgram tls = mapM runTLStmt tls

runTLStmt :: TLStmt a -> State Vars SymbolTree 
runTLStmt (FuncDef ident stmts) = error "TODO"
runTLStmt _ = error "TODO"

{-
:: Ident -> [Stmt a] -> TLStmt () 
    TLAssign  :: Expr a -> TLStmt ()
       
    TLStmtReturn :: a -> TLStmt a
    TLStmtBind :: TLStmt a -> (a -> TLStmt b) -> TLStmt b    
-}
