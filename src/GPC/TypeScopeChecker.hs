{-# LANGUAGE TemplateHaskell #-}

{- Check types and scope of identifiers -}
module GPC.TypeScopeChecker(
    getTypeExpr,injectConstants, reduceExpr, runTypeChecker) where


import qualified Data.Map as M
import Data.Bits
import Control.Applicative hiding ((<|>), many, optional, empty)
import Control.Monad.Error
import Control.Monad.State.Lazy
import Control.Error.Util
import Control.Lens
import GPC.AST

type VarTable = M.Map Ident Type
type ConstVarTable = M.Map Ident Literal
type FunTable = M.Map Ident (Type, [Type])

boolType = Type "bool"
intType = Type "int"
strType = Type "string"
chType = Type "char"
doubleType = Type "double"

data MainBlock = MainBlock {
    _funcs :: M.Map Ident CodeBlock, -- ^ Function Blocks
    _tlFuncDefs :: FunTable, -- ^ Function Definitions
    _tlConstVars :: ConstVarTable, -- ^ Top Level Constant Variable values
    _tlConstVarTypes :: VarTable -- ^ Top Level Constant variable types
} deriving (Show)

data CodeBlock = CodeBlock {
    _funcDefs :: FunTable, -- ^ Function names and return/argument types
    _prevVars :: VarTable, -- ^ Identifiers visible in current scope with types
    _curVars :: VarTable , -- ^ Identifiers declared in current scope
    _constVars :: ConstVarTable , -- ^ Identifiers visible from current scope which
                                        -- ^ evaluate to a constant value
    _subBlocks :: [CodeBlock] -- ^ Sub Blocks
} deriving (Show)

-- Create lenses to access Block fields easier
makeLenses ''MainBlock 
makeLenses ''CodeBlock

-- Monad Transformer combining State with Either
-- when doing type checking if a failure occurs
-- we can return an error String
type CodeState a = StateT MainBlock (Either String) a
type BlockState a = StateT CodeBlock (Either String) a

-- | Perform Type/Scope checking
runTypeChecker :: Program -> Either String MainBlock
runTypeChecker (Program tls) = case runStateT (evalTLStmts tls) initialBlock of
 Left s -> Left s
 (Right ((), codeSt)) -> Right codeSt
 where initialBlock = MainBlock M.empty M.empty M.empty M.empty

-- | Type Check all top level statements
evalTLStmts :: [TopLevel] -> CodeState()
evalTLStmts tls = mapM_ evalTLStmt tls

-- | Type check a given top level statement
evalTLStmt :: TopLevel -> CodeState ()
evalTLStmt tl = case tl of
    (TLAssign assign) -> evalTLAssign assign
    (Func gType ident args stmts) -> evalFunc gType ident args stmts
     
-- | Type Check top level assignment
evalTLAssign :: Assign -> CodeState ()
evalTLAssign (Assign typeG ident expr) = do
    cVars <- use tlConstVars
    tVars <- use tlConstVarTypes
    reducedExpr <- lift $ reduceExpr tVars $ injectConstants cVars expr
 
    case reducedExpr of
        (ExpLit l) -> do

            exprType <- lift $ getTypeExpr tVars M.empty reducedExpr 
            -- Check Types match and Variable is Single instance
            if exprType == typeG then 
                if ident `M.notMember` tVars then do -- Update State
                  assign tlConstVars $ M.insert ident l cVars
                  assign tlConstVarTypes $ M.insert ident typeG tVars
                else multipleInstance 
            else conflictingTypes exprType

        otherwise -> notConstant
                           
 where 
    multipleInstance = lift $ Left $ (show ident) ++ " has already been defined " ++ 
        "in scope, cannot redefine it" 
    conflictingTypes exprType = lift $ Left $ show (ident) ++ "is defined as type" ++ 
        (show typeG) ++ " but assignment evaluates to type " ++ (show exprType)
    notConstant = lift $ Left $ "Top level assignment are expected to be constant, " ++ 
        (show ident) ++ "is not constant"
                          
evalFunc :: Type -> Ident -> [(Type, Ident)] -> BlockStmt -> CodeState()
evalFunc typeG ident args (BlockStmt stmts) = do
    funs <- use funcs
    funcDefs <- use tlFuncDefs
    constVars <- use tlConstVars
    varTypes <- use tlConstVarTypes
    -- Check function isn't already defined
    if ident `M.notMember` funcDefs
        then do
            let newBlock = CodeBlock funcDefs varTypes M.empty constVars []            
            assign tlFuncDefs $ M.insert ident (typeG, map fst args) funcDefs
            funBlock <- lift $ runBlockCheck stmts newBlock
            assign funcs $ M.insert ident funBlock funs
        else lift $ Left $ "Function " ++ show (ident) ++ "occurs more than once"

-- | Run Type Checker on new code block
runBlockCheck :: [Stmt] -> CodeBlock -> Either String CodeBlock
runBlockCheck stmts cb =  case runStateT (evalStmts stmts) cb of
    Left s -> Left s
    (Right ((), codeSt)) -> Right codeSt

-- | Type Check all statements in the current scope
evalStmts :: [Stmt] -> BlockState()
evalStmts tls = mapM_ evalStmt tls

-- | Type check given statement    
evalStmt :: Stmt -> BlockState ()
evalStmt stmt = case stmt of
   (AssignStmt assign) -> checkAssign assign
    

-- |Type Check Assignment Statement
checkAssign :: Assign -> BlockState()
checkAssign (Assign gType ident expr) = do
    ftable <- use funcDefs
    oldVtable <- use prevVars
    vTable <- use curVars
    cTable <- use constVars
    let scopeVars = vTable `M.union` oldVtable -- Gives all visible identifiers
    expr <- lift $ reduceExpr scopeVars $ injectConstants cTable expr
    if ident `M.member` vTable then do
        exprType <- lift $ getTypeExpr scopeVars ftable expr 
        if gType == exprType then do
            assign curVars   $ M.insert ident gType vTable
            -- Update Const Table
            assign constVars $ case expr of 
                            (ExpLit l) -> M.insert ident l cTable
                            otherwise -> M.delete ident cTable
        else typeMismatch gType exprType
    else redefine
 where
    redefine = lift $ Left $ "Error, cannot redefine " ++ (show ident) ++ " in current scope" 
    typeMismatch l r = lift $ Left $ (show ident) ++ " declared as type " ++ (show l) ++
                            "but rhs evaluates to type " ++ (show r) 

-- |Type Check If Statement
checkIf :: Expr -> Stmt -> BlockState()
checkIf expr stmt = do
    fTable <- use funcDefs
    scopeVars <- M.union <$> use curVars <*> use prevVars 
    exprType <- lift $ getTypeExpr scopeVars fTable expr
    if exprType == boolType then
        evalStmt stmt
    else lift $ Left $ "Expression within if is expected to be a bool " ++
        " but it evaluates to a " ++ (show exprType)

-- | Obtain Type of Expression, returns error message
-- | if types arn't consistent, or identifiers arn't in scope
getTypeExpr :: VarTable -> FunTable -> Expr -> Either String Type
getTypeExpr vtable ftable expr = case expr of
    (ExpBinOp b e1 e2) -> getTypeBinOp b e1 e2
    (ExpUnaryOp u e) -> getTypeUnOp u e
    (ExpFunCall (FunCall s exps)) -> do
        argTypes <- mapM (getTypeExpr vtable ftable) exps
        (retT, ts) <- note (notFound s) (M.lookup s ftable)
        if (length argTypes) /= (length ts)
            then Left $ "Function " ++ (show s) ++  " expects " ++ (show $ length ts) ++
                      " arguments but was given " ++ (show $ length argTypes)
            else if argTypes /= ts 
                then Left "Arguments don't evaluate to given types"
                else Right retT

    (ExpIdent i) -> note (notFound i) (M.lookup i vtable) 
    (ExpLit l) -> return $ Type $ case l of
                Str s -> "string"
                Ch c -> "char"
                Number (Left i) -> "int"
                Number (Right d) -> "double"
                Bl b -> "bool"

 where notFound (Ident i) = "Identifier " ++ i ++ "not declared in scope"
       getTypeBinOp :: BinOps -> Expr -> Expr -> Either String Type
       getTypeBinOp bop e1 e2 
           | bop `elem` numNumNumOp = do
               leftType  <- getTypeExpr vtable ftable e1
               rightType <- getTypeExpr vtable ftable e2
               if leftType /= rightType 
                   then Left "Both expressions expected to be the same type"
                   else case leftType of
                       (Type "int") -> return $ Type "int"
                       (Type "double") -> return $ Type "double"
                       otherwise -> Left $ "Expected integer or double type"

           | bop `elem` intIntIntOp = do                
               leftType  <- getTypeExpr vtable ftable e1
               rightType <- getTypeExpr vtable ftable e2
               case (leftType, rightType) of
                   (Type "int", Type "int") -> return $ Type "int"
                   otherwise -> Left $ "Expected integer values"      

           | bop `elem` compareOp = do
               leftType  <- getTypeExpr vtable ftable e1
               rightType <- getTypeExpr vtable ftable e2
               case (leftType, rightType) of
                   (Type "int", Type "int") -> return $ Type "bool"
                   (Type "double", Type "double") -> return $ Type "bool"
                   otherwise -> Left $ "Expected numeric values of the same type"      

           | bop `elem` boolOp = do
               leftType  <- getTypeExpr vtable ftable e1
               rightType <- getTypeExpr vtable ftable e2
               case (leftType, rightType) of
                   (Type "bool", Type "bool") -> return $ Type "bool"
                   otherwise -> Left $ "Expected boolean values"      

       numNumNumOp = [Add, Sub, Mul, Div]        
       intIntIntOp = [Mod, BAnd, BOr, BXor, ShiftL, ShiftR]
       compareOp = [LessEq, Less, Equals, Greater, GreaterEq]
       boolOp = [And, Or]

       getTypeUnOp :: UnaryOps -> Expr -> Either String Type
       getTypeUnOp op e 
        | op == Not || op == Neg = getTypeExpr vtable ftable e >>= 
            \t -> case t of 
                (Type "int") -> return $ Type "int"
                otherwise -> Left "Expected integer expression"
        | op == BNot = getTypeExpr vtable ftable e >>=
            \t -> case t of 
                (Type "bool") -> return $ Type "bool"
                otherwise -> Left "Expected boolean expression"

        


-- Replace all constant identifiers with their
-- constant value
injectConstants :: ConstVarTable -> Expr -> Expr
injectConstants ctable expr = case expr of
    (ExpBinOp b e1 e2) -> ExpBinOp b (injectConstants ctable e1) (injectConstants ctable e2)
    (ExpUnaryOp u e) -> ExpUnaryOp u (injectConstants ctable e)
    (ExpFunCall (FunCall s exps)) -> ExpFunCall (FunCall s (map (injectConstants ctable) exps))
    (ExpIdent i) -> case M.lookup i ctable of
                                Just l ->   ExpLit l
                                Nothing ->  ExpIdent i
    (ExpLit l) -> (ExpLit l)



-- |Attempts to reduce an expression as much as possible
-- |Returns an error string if evaluated expression 
-- |is invalid or an identifier is not present in the given table
-- |otherwise returns the reduced expression
reduceExpr :: VarTable -> Expr -> Either String Expr
reduceExpr vtable expr = case expr of
    (ExpBinOp b e1 e2) -> do 
        re1 <- reduceExpr vtable e1
        re2 <- reduceExpr vtable e2 
        evaluateBinExpr b re1 re2

    (ExpUnaryOp u e) -> do 
        re <- reduceExpr vtable e
        evaluateUnExpr u re

    (ExpFunCall (FunCall s exps)) -> do
         rexps <- mapM (reduceExpr vtable) exps
         return $ ExpFunCall (FunCall s rexps)

    (ExpIdent i) -> ExpIdent <$> if M.member i vtable 
                                    then (Right i) 
                                    else (Left $ notFound i)

    (ExpLit l) -> return (ExpLit l)
 where notFound (Ident i) = "Identifier " ++ i ++ "not declared in scope"


-- |Attempts to evaluate a constant binary expression, checks the types as well
evaluateBinExpr :: BinOps -> Expr -> Expr -> Either String Expr
evaluateBinExpr b (ExpLit l1) (ExpLit l2) = (binOpTable b) l1 l2
     
evaluateBinExpr  b e1 e2 = return $ ExpBinOp b e1 e2

-- |Obtain binary operation to use with literal values
binOpTable :: BinOps -> (Literal -> Literal -> Either String Expr)
binOpTable b = case b of
    Add -> performBinNumOp (+)
    Sub -> performBinNumOp (-)
    Div -> performBinNumOp (/)
    Mul -> performBinNumOp (*)
    

    Mod -> performBinIntOp mod
    BAnd -> performBinIntOp (.&.) 
    BOr -> performBinIntOp (.|.)
    BXor -> performBinIntOp xor
    ShiftL -> performBinIntOp (\a b ->  shift a $ fromIntegral b)
    ShiftR -> performBinIntOp (\a b ->  shift a $ fromIntegral (-b))

    Less -> performBinCompareOp (<)
    LessEq -> performBinCompareOp (<=)
    Greater -> performBinCompareOp (>)
    GreaterEq -> performBinCompareOp (>=)
    Equals -> performBinCompareOp (==)

    And -> performBinBoolOp (&&) 
    Or -> performBinBoolOp (||)


performBinNumOp :: (Double  -> Double -> Double)  -> Literal -> Literal -> Either String Expr
performBinNumOp op (Number (Left n1)) (Number (Left n2)) = Right litExp
 where litExp = ExpLit $ Number $ Left $ truncate $ n1' `op` n2'
       n1' = fromIntegral n1
       n2' = fromIntegral n2

performBinNumOp op (Number (Right n1))(Number (Right n2)) = Right $ ExpLit $ Number $ Right $ n1 `op` n2
performBinNumOp b _ _ = Left "Error expected a numeric value" 

performBinIntOp :: (Integer -> Integer -> Integer)  -> Literal -> Literal -> Either String Expr
performBinIntOp op (Number (Left n1)) (Number (Left n2)) = 
    Right $ ExpLit $ Number $ Left $ n1 `op` n2 
performBinIntOp op _ _ = Left "Error expected integer types"
    
performBinCompareOp :: (Double -> Double -> Bool) -> Literal -> Literal -> Either String Expr
performBinCompareOp op (Number (Left n1)) (Number (Left n2)) = Right $ ExpLit $ Bl $ n1' `op` n2'
 where n1' = fromIntegral n1
       n2' = fromIntegral n2

performBinCompareOp op (Number (Right n1)) (Number (Right n2)) = Right $ ExpLit $ Bl $ n1 `op` n2 
performBinCompareOp _ _ _ = Left "Error expected either 2 ints, or 2 doubles"
    
performBinBoolOp :: (Bool -> Bool -> Bool) -> Literal -> Literal -> Either String Expr
performBinBoolOp op (Bl b1) (Bl b2) = Right $ ExpLit $ Bl $ b1 `op` b2
performBinBoolOp _ _ _ = Left "Error expected boolean values"

-- |Attempts to evaluate a constant unary expression, check the types as
-- |well
evaluateUnExpr :: UnaryOps -> Expr -> Either String Expr
evaluateUnExpr unOp (ExpLit l) = (unOpTable unOp) l
evaluateUnExpr unOp expr = Right expr

unOpTable u = case u of
    Not -> performUnNotOp
    Neg -> performUnNegOp
    BNot -> performUnBNotOp

performUnNotOp ::  Literal -> Either String Expr
performUnNotOp (Bl b1) = Right $ ExpLit $ Bl $ not b1
performUnNotOp _ = Left "Error expected boolean value"

performUnNegOp :: Literal -> Either String Expr
performUnNegOp (Number (Left i)) =  Right $ ExpLit $ Number $ Left  $ negate i
performUnNegOp (Number (Right i)) = Right  $ ExpLit $ Number $ Right $ negate i
performUnNegOp _ = Left "Error expected numeric type"

performUnBNotOp :: Literal -> Either String Expr
performUnBNotOp (Number (Left i)) = Right $ ExpLit $ Number $ Left $ complement i
performUnBNotOp _ = Left "Error expected integer value"

