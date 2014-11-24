{-# LANGUAGE TemplateHaskell #-}

{- Check types and scope of identifiers -}
module GPC.TypeScopeChecker(
    getTypeExpr,injectConstants, reduceExpr, runTypeChecker) where


import qualified Data.Map as M
import Data.Bits
import Control.Applicative hiding ((<|>), many, optional, empty)
import Control.Monad.Except
import Control.Monad.State.Lazy
import Control.Error.Util
import Control.Lens
import GPC.AST

type VarTable = M.Map Ident Type
type ConstVarTable = M.Map Ident Literal
type FunTable = M.Map Ident (Type, [Type])


boolType = Type "bool"
intType = Type "int"
--strType = Type "string"
--chType = Type "char"
--doubleType = Type "double"


data MainBlock = MainBlock {
    _funcs :: M.Map Ident CodeBlock, -- ^ Function Blocks
    _tlFuncDefs :: FunTable, -- ^ Function Definitions
    _tlConstVars :: ConstVarTable, -- ^ Top Level Constant Variable values
    _tlConstVarTypes :: VarTable -- ^ Top Level Constant variable types
} deriving (Show)


data CodeBlock = CodeBlock {
    _currentFun :: Ident, -- ^ Name of Function block is in
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
    (TLAssign a) -> evalTLAssign a
    (Func gType ident args stmts) -> evalFunc gType ident args stmts
   -- (TLObjs objects) -> 
    _ -> lift $ Left $ "Not implemented"    

-- | Type check object declarations
--evalObjs :: Objects -> CodeState ()
--evalObjs obj = do 
--    cVars <- use tlConstVars
--    tVars <- use tlConstVarTypes
--    case obj of
--        (Obj1 gClass ident) -> do
--            if ident `M.notMember` tVars then do
--                assign tlConstVarTypes $ M.insert ident (Type $ show gClass) tVars 
--        (ObjM gClass ident exp) -> do
         
     
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

        _ -> notConstant
                           
 where 
    multipleInstance = lift $ Left $ (show ident) ++ " has already been defined " ++ 
        "in scope, cannot redefine it" 
    conflictingTypes exprType = lift $ Left $ show (ident) ++ "is defined as type" ++ 
        (show typeG) ++ " but assignment evaluates to type " ++ (show exprType)
    notConstant = lift $ Left $ "Top level assignment are expected to be constant, " ++ 
        (show ident) ++ "is not constant"


-- | Type check Function                          
evalFunc :: Type -> Ident -> [(Type, Ident)] -> BlockStmt -> CodeState()
evalFunc typeG ident args (BlockStmt stmts) = do
    funs <- use funcs
    fTable <- use tlFuncDefs
    cVars <- use tlConstVars
    varTypes <- use tlConstVarTypes
    -- Check function isn't already defined
    if ident `M.notMember` fTable
        then do
            let newFTable = M.insert ident (typeG, map fst args) fTable
            let newBlock = CodeBlock ident newFTable varTypes M.empty cVars []            
            assign tlFuncDefs newFTable
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
   (AssignStmt a) -> checkAssign a
   (If expr stmt') -> checkIf expr stmt'
   (IfElse expr stmt1 stmt2) -> checkIfElse expr stmt1 stmt2
   (Seq blockStmt) -> checkBlock blockStmt M.empty
   (BStmt blockStmt) -> checkBlock blockStmt M.empty
   (Return expr) -> checkReturn expr
   (ForLoop ident expr1 expr2 expr3 stmts) -> checkForLoop ident expr1 expr2 expr3 stmts
   (MethodStmt method) -> checkMethodCall method
   _ -> lift $ Left $ "Not implemented"

-- |Type Check Assignment Statement
checkAssign :: Assign -> BlockState()
checkAssign (Assign gType ident expr) = do
    ftable <- use funcDefs
    oldVtable <- use prevVars
    vTable <- use curVars
    cTable <- use constVars
    let scopeVars = vTable `M.union` oldVtable -- Gives all visible identifiers
    expr' <- lift $ reduceExpr scopeVars $ injectConstants cTable expr
    if ident `M.member` vTable then do
        exprType <- lift $ getTypeExpr scopeVars ftable expr'
        if gType == exprType then do
            -- Update Var table with new variable
            assign curVars   $ M.insert ident gType vTable
            -- Update Const Table
            assign constVars $ case expr of 
                            (ExpLit l) -> M.insert ident l cTable
                            _ -> M.delete ident cTable
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

-- |Type Check If - Else Statement
checkIfElse :: Expr -> Stmt -> Stmt -> BlockState()
checkIfElse expr thenStmt elseStmt = do
    checkIf expr thenStmt
    evalStmt elseStmt

-- | Type check for loop
checkForLoop :: Ident -> Expr -> Expr -> Expr -> BlockStmt -> BlockState()
checkForLoop ident startExpr stopExpr stepExpr blockStmt = do
    cTable <- use constVars
    fTable <- use funcDefs
    scopeVars <- M.union <$> use curVars <*> use prevVars 

    startExpr' <- lift $ reduceExpr scopeVars $ injectConstants cTable startExpr
    stopExpr' <- lift $ reduceExpr scopeVars $ injectConstants cTable stopExpr
    stepExpr' <- lift $ reduceExpr scopeVars $ injectConstants cTable stepExpr

    -- Check all expressions are constant (for loops are static)
    -- Check types of each expression are all integers
    -- Then type check the for block
    let exprs = [startExpr', stopExpr', stepExpr']
    mapM_ checkConstantExpr exprs
    types <- lift $ mapM (getTypeExpr scopeVars fTable) exprs
    mapM_ (checkType intType) types

    checkBlock blockStmt (M.singleton ident intType)
     

-- | Type check inner block, add to current list of inner blocks
checkBlock :: BlockStmt -> VarTable -> BlockState()
checkBlock (BlockStmt stmts) innerTable = do
    fName <- use currentFun
    fTable <- use funcDefs
    cTable <- use constVars
    innerBlocks <- use subBlocks
    scopeVars <- M.union <$> use curVars <*> use prevVars 
    
    -- Create and type check new inner block, and add to current
    -- list of inner blocks if successful
    let newBlock = CodeBlock fName fTable scopeVars innerTable cTable []   
    subBlock <- lift $ runBlockCheck stmts newBlock         
    assign subBlocks $ innerBlocks ++ [subBlock]


-- | Type check return stmt
checkReturn :: Expr -> BlockState()
checkReturn expr = do
    fName <- use currentFun
    fTable <- use funcDefs
    scopeVars <- M.union <$> use curVars <*> use prevVars 

    let notFound = "Error, function not found " ++ show (fName)

    (retType, _) <- lift $ note notFound $ M.lookup fName fTable
    exprType <- lift $ getTypeExpr scopeVars fTable expr

    if retType == exprType then
        modify id -- Type checking return doesn't modify state, return old state
    else
        lift $ Left $ "The return type of function " ++ (show fName) ++
            "is " ++ (show retType) ++ "but return expression evaluates to" ++
            "type " ++ (show exprType)

-- | Type check method call
checkMethodCall :: MethodCall -> BlockState()
checkMethodCall _ = modify id

checkConstantExpr :: Expr -> BlockState()
checkConstantExpr expr = case expr of
    (ExpLit _) -> modify id
    _ -> lift $ Left $ "expected constant expression"

checkType :: Type -> Type -> BlockState()
checkType expected actual = 
    if expected == actual then modify id else lift $ Left $ "Expected type " ++ 
        (show expected) ++ " but expression evaluated to " ++ (show actual)

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

    (ExpMethodCall _) -> return $ Type "Object"
    (ExpIdent i) -> note (notFound i) (M.lookup i vtable) 
    (ExpLit l) -> return $ Type $ case l of
                Str _ -> "string"
                Ch _ -> "char"
                Number (Left _) -> "int"
                Number (Right _) -> "double"
                Bl _ -> "bool"

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
                       _ -> Left $ "Expected integer or double type"

           | bop `elem` intIntIntOp = do                
               leftType  <- getTypeExpr vtable ftable e1
               rightType <- getTypeExpr vtable ftable e2
               case (leftType, rightType) of
                   (Type "int", Type "int") -> return $ Type "int"
                   _ -> Left $ "Expected integer values"      

           | bop `elem` compareOp = do
               leftType  <- getTypeExpr vtable ftable e1
               rightType <- getTypeExpr vtable ftable e2
               case (leftType, rightType) of
                   (Type "int", Type "int") -> return $ Type "bool"
                   (Type "double", Type "double") -> return $ Type "bool"
                   _ -> Left $ "Expected numeric values of the same type"      

           | bop `elem` boolOp = do
               leftType  <- getTypeExpr vtable ftable e1
               rightType <- getTypeExpr vtable ftable e2
               case (leftType, rightType) of
                   (Type "bool", Type "bool") -> return $ Type "bool"
                   _ -> Left $ "Expected boolean values"    
                     
            | otherwise = Left $ "Compiler error during obtaining type of binary expression"
       numNumNumOp = [Add, Sub, Mul, Div]        
       intIntIntOp = [Mod, BAnd, BOr, BXor, ShiftL, ShiftR]
       compareOp = [LessEq, Less, Equals, Greater, GreaterEq]
       boolOp = [And, Or]

       getTypeUnOp :: UnaryOps -> Expr -> Either String Type
       getTypeUnOp operation e 
        | operation == Not || operation == Neg = getTypeExpr vtable ftable e >>= 
            \t -> case t of 
                (Type "int") -> return $ Type "int"
                _ -> Left "Expected integer expression"
        | operation == BNot = getTypeExpr vtable ftable e >>=
            \t -> case t of 
                (Type "bool") -> return $ Type "bool"
                _ -> Left "Expected boolean expression"
        
        | otherwise = Left $ "Compiler error during obtaining type of unary expression"


-- Replace all constant identifiers with their
-- constant value
injectConstants :: ConstVarTable -> Expr -> Expr
injectConstants ctable expr = case expr of
    (ExpBinOp b e1 e2) -> ExpBinOp b (injectConstants ctable e1) (injectConstants ctable e2)
    (ExpUnaryOp u e) -> ExpUnaryOp u (injectConstants ctable e)
    (ExpFunCall (FunCall s exps)) -> ExpFunCall (FunCall s (map (injectConstants ctable) exps))
    (ExpMethodCall (MethodCall obj method args)) -> 
        ExpMethodCall (MethodCall obj method (map (injectConstants ctable) args))
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
        reducedExpr <- reduceExpr vtable e
        evaluateUnExpr u reducedExpr

    (ExpFunCall (FunCall s exps)) -> do
         rexps <- mapM (reduceExpr vtable) exps
         return $ ExpFunCall (FunCall s rexps)

    (ExpMethodCall (MethodCall obj method args)) -> do
        rexps <- mapM (reduceExpr vtable) args
        return $ ExpMethodCall (MethodCall obj method rexps)

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
    ShiftL -> performBinIntOp (\x y ->  shift x $ fromIntegral y)
    ShiftR -> performBinIntOp (\x y ->  shift x $ fromIntegral (-y))

    Less -> performBinCompareOp (<)
    LessEq -> performBinCompareOp (<=)
    Greater -> performBinCompareOp (>)
    GreaterEq -> performBinCompareOp (>=)
    Equals -> performBinCompareOp (==)
    NEquals -> performBinCompareOp (/=)

    And -> performBinBoolOp (&&) 
    Or -> performBinBoolOp (||)


performBinNumOp :: (Double  -> Double -> Double)  -> Literal -> Literal -> Either String Expr
performBinNumOp operation (Number (Left n1)) (Number (Left n2)) = Right litExp
 where litExp = ExpLit $ Number $ Left $ truncate $ n1' `operation` n2'
       n1' = fromIntegral n1
       n2' = fromIntegral n2

performBinNumOp operation (Number (Right n1))(Number (Right n2)) = 
    Right $ ExpLit $ Number $ Right $ n1 `operation` n2
performBinNumOp _ _ _ = Left "Error expected a numeric value" 

performBinIntOp :: (Integer -> Integer -> Integer)  -> Literal -> Literal -> Either String Expr
performBinIntOp operation (Number (Left n1)) (Number (Left n2)) = 
    Right $ ExpLit $ Number $ Left $ n1 `operation` n2
performBinIntOp _ _ _ = Left "Error expected integer types"
    
performBinCompareOp :: (Double -> Double -> Bool) -> Literal -> Literal -> Either String Expr
performBinCompareOp operation (Number (Left n1)) (Number (Left n2)) = 
    Right $ ExpLit $ Bl $ n1' `operation` n2'
 where n1' = fromIntegral n1
       n2' = fromIntegral n2

performBinCompareOp operation (Number (Right n1)) (Number (Right n2)) = 
    Right $ ExpLit $ Bl $ n1 `operation` n2 
performBinCompareOp _ _ _ = Left "Error expected either 2 ints, or 2 doubles"
    
performBinBoolOp :: (Bool -> Bool -> Bool) -> Literal -> Literal -> Either String Expr
performBinBoolOp operation (Bl b1) (Bl b2) = 
    Right $ ExpLit $ Bl $ b1 `operation` b2
performBinBoolOp _ _ _ = Left "Error expected boolean values"

-- |Attempts to evaluate a constant unary expression, check the types as
-- |well
evaluateUnExpr :: UnaryOps -> Expr -> Either String Expr
evaluateUnExpr unOp (ExpLit l) = (unOpTable unOp) l
evaluateUnExpr _ e = Right e

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

