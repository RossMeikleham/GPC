{-# LANGUAGE TemplateHaskell #-}

module GPC.Interpreter where

import qualified Data.Map as M
import           Control.Applicative hiding (empty, many, optional, (<|>))
import           Control.Lens
import           Control.Monad.State.Lazy
import           Control.Error.Util

import           GPC.TypelessAST         


data Environment = Environment {
    _exprTable :: M.Map String Expr
}

type EnvState a = StateT Environment (Either String) a

-- Create lenses to access Block fields easier
makeLenses ''Environment




getExpr :: String -> EnvState Expr
getExpr s = do 
    vars <- use exprTable 
    lift $ note notFound $ M.lookup s vars
  where notFound = "Identifier not found " ++ s 


setExpr :: String -> Expr -> EnvState ()
setExpr s expr = do
    vars <- use exprTable
    assign exprTable $ M.insert s expr vars

reduceExpr :: Expr -> EnvState Expr
reduceExpr expr = case expr of
    ExpBinOp bOp e1 e2 -> do
        e1' <- reduceExpr e1
        e2' <- reduceExpr e2
        evalBinOp bOp e1' e2'

    ExpUnaryOp op e -> do
        e' <- reduceExpr e
        evalUnOp op e'

    ExpFunCall fc -> reduceFunCall fc

    ExpMethodCall mc -> reduceMethodCall mc

    ExpIdent (Ident s) -> do
    --TODO relook at
        e <- getExpr s
        reduceExpr e

    ExpLit l -> return $ ExpLit l

reduceFunCall :: FunCall -> EnvState Expr
reduceFunCall (FunCall name exprs) = do
    exprs' <- mapM reduceExpr exprs
    return $ ExpFunCall $ FunCall name exprs'

reduceMethodCall :: MethodCall -> EnvState Expr
reduceMethodCall (MethodCall var name exprs) = do
    exprs' <- mapM reduceExpr exprs
    var' <- case var of
            VarArrayElem name expr -> (VarArrayElem name) `fmap` (reduceExpr expr)
            VarIdent name -> return $ VarIdent name
    return $ ExpMethodCall $ MethodCall var' name exprs'

evalBinOp :: BinOps -> Expr -> Expr -> EnvState Expr
evalBinOp op e1 e2 = error "todo"


evalUnOp :: UnaryOps -> Expr -> EnvState Expr
evalUnOp op e1 = error "todo"


