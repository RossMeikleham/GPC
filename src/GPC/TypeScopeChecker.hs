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
type PtrTable = M.Map Ident Pointer

boolType = flip NormalType "bool"
intType = flip NormalType "int"
strType = flip NormalType "string"
chType = flip NormalType "char"
doubleType = flip NormalType "double"


-- Upcast a type to a Kernel type, if
-- the given type is already a Kernel type then
-- returns the given type
castToKernel :: Type -> Type
castToKernel (PointerType t) = PointerType $ castToKernel t
castToKernel (NormalType _ n) = NormalType True n

isInKernel :: Type -> Bool
isInKernel (PointerType t) = isInKernel t
isInKernel (NormalType k _) = k

notKernel = False
inKernel = True


isPointer :: Type -> Bool
isPointer (PointerType _) = True
isPointer _ = False

data MainBlock = MainBlock {
    _tlFuncDefs :: FunTable, -- ^ Function Definitions
    _tlConstVars :: ConstVarTable, -- ^ Top Level Constant Variable values
    _tlConstVarTypes :: VarTable, -- ^ Top Level Constant variable types
    _tlPtrs :: PtrTable
} deriving (Show)


data CodeBlock = CodeBlock {
    _currentFun :: Ident, -- ^ Name of Function block is in
    _funcDefs :: FunTable, -- ^ Function names and return/argument types
    _prevVars :: VarTable, -- ^ Identifiers visible in current scope with types
    _curVars :: VarTable , -- ^ Identifiers declared in current scope
    _pointers :: PtrTable, -- ^ Pointers declared in current scope
    _constVars :: ConstVarTable  -- ^ Identifiers visible from current scope which
                                        -- ^ evaluate to a constant value
} deriving (Show)


-- Create lenses to access Block fields easier
makeLenses ''MainBlock 
makeLenses ''CodeBlock


-- Monad Transformer combining State with Either
-- when doing type checking if a failure occurs
-- we can return an error String
type GenericBlockState a b = StateT a (Either String) b
type CodeState a = GenericBlockState MainBlock a
type BlockState a = GenericBlockState CodeBlock a


-- | Perform Type/Scope checking, and simple expression reduction
-- | Returns either an error message or the Reduced GPC AST
runTypeChecker :: Program -> Either String Program
runTypeChecker (Program tls) = case runStateT (evalTLStmts tls) initialBlock of
 Left s -> Left s
 (Right (tl, _)) -> Right $ Program tl
 where initialBlock = MainBlock M.empty M.empty M.empty M.empty


-- | Type Check all top level statements
evalTLStmts :: [TopLevel] -> CodeState [TopLevel]
evalTLStmts = mapM evalTLStmt


-- | Type check a given top level statement
evalTLStmt :: TopLevel -> CodeState TopLevel
evalTLStmt tl = case tl of
    (TLAssign a) -> TLAssign <$> evalTLAssign a
    (Func gType ident args stmts) -> evalFunc gType ident args stmts
    (TLObjs objects) -> TLObjs <$> evalObjs objects
    (TLConstructObjs cObjs) -> TLConstructObjs <$> evalConstruct cObjs


-- | Type check object initializations
evalConstruct :: ConstructObjs -> CodeState ConstructObjs
evalConstruct (ConstructObjs ns var exprs) = do
    cVars <- use tlConstVars
    tVars <- use tlConstVarTypes

    -- Check expressions for Constructor are constant 
    reducedExprs <- lift $ mapM (reduceExpr tVars . injectConstants cVars) exprs
    _ <- lift $ mapM (getTypeExpr tVars M.empty) reducedExprs
    mapM_ checkConstantExpr reducedExprs

    case var of 
        (VarIdent _) ->  
            return $ ConstructObjs ns var reducedExprs

        (VarArrayElem ident expr) -> do -- Check indexed expression           
            reducedExpr <- lift $ reduceExpr tVars $ injectConstants cVars expr
            exprType <- lift $ getTypeExpr tVars M.empty reducedExpr
            checkType (intType notKernel) exprType
            checkConstantExpr reducedExpr
            return $ ConstructObjs ns (VarArrayElem ident reducedExpr) reducedExprs


-- | Type check object declarations
evalObjs :: Objects -> CodeState Objects
evalObjs objs@(Objects _ var) = do 
    cVars <- use tlConstVars
    tVars <- use tlConstVarTypes
    
    case var of
        -- Single Object, check identifier isn't already in scope
        (VarIdent ident) -> 
            if ident `M.notMember` tVars then do
                assign tlConstVarTypes $ M.insert ident (NormalType inKernel "object") tVars 
                return objs
            else multipleInstance ident

        -- Static Array of Objects, check type of array size, check size
        -- is a constant, and that that identifier for the array isn't already in scope
        (VarArrayElem ident expr) -> 
            if ident `M.notMember` tVars then do 
                reducedExpr <- lift $ reduceExpr tVars $ injectConstants cVars expr
                exprType <- lift $ getTypeExpr tVars M.empty reducedExpr
                checkType (intType notKernel) exprType
                checkConstantExpr reducedExpr
                assign tlConstVarTypes $ M.insert ident (NormalType inKernel "objArray") tVars
                return $ objs {objVar = VarArrayElem ident reducedExpr}
            else multipleInstance ident
 where          
    multipleInstance ident = lift $ Left $ show ident ++ " has already been defined " ++ 
        "in scope, cannot redefine it"      


-- | Type Check top level assignment
evalTLAssign :: Assign -> CodeState Assign
evalTLAssign (Assign typeG ident expr) = do
    cVars <- use tlConstVars
    tVars <- use tlConstVarTypes
    ptrs <- use tlPtrs
    reducedExpr' <- lift $ reduceExpr tVars $ injectConstants cVars $ injectPtrs ptrs expr
    let reducedExpr = injectConstants cVars reducedExpr'
    exprType <- lift $ getTypeExpr tVars M.empty reducedExpr 
 
    case reducedExpr of
        (ExpLit l) -> do

            -- Check Types match and Variable is Single instance
            checkType exprType typeG
            if ident `M.notMember` tVars then do -- Update State
                assign tlConstVars $ M.insert ident l cVars
                assign tlConstVarTypes $ M.insert ident typeG tVars
                return $ Assign typeG ident reducedExpr
            else multipleInstance 

        _ -> notConstant
                           
 where 
    multipleInstance = lift $ Left $ show ident ++ " has already been defined " ++ 
        "in scope, cannot redefine it" 
    notConstant = lift $ Left $ "Top level assignment are expected to be constant, " ++ 
        show ident ++ "is not constant"


-- | Type check Function                          
evalFunc :: Type -> Ident -> [(Type, Ident)] -> BlockStmt -> CodeState TopLevel
evalFunc typeG ident args (BlockStmt stmts) = do
    fTable <- use tlFuncDefs
    cVars <- use tlConstVars
    varTypes <- use tlConstVarTypes
    ptrs <- use tlPtrs 
    -- Check function isn't already defined
    if ident `M.notMember` fTable
        then do
            let newFTable = M.insert ident (typeG, map fst args) fTable
            let newBlock = CodeBlock ident newFTable varTypes M.empty ptrs cVars          
            assign tlFuncDefs newFTable
            funBlock <- lift $ runBlockCheck stmts newBlock
           -- assign funcs $ M.insert ident funBlock funs
            return $ Func typeG ident args funBlock
        else lift $ Left $ "Function " ++ show ident ++ "occurs more than once"


-- | Run Type Checker on new code block
runBlockCheck :: [Stmt] -> CodeBlock -> Either String BlockStmt
runBlockCheck stmts cb =  case runStateT (evalStmts stmts) cb of
    Left s -> Left s
    (Right (stmts', _)) -> Right $ BlockStmt stmts'


-- | Type Check all statements in the current scope
evalStmts :: [Stmt] -> BlockState [Stmt]
evalStmts = mapM evalStmt


-- | Type check given statement    
evalStmt :: Stmt -> BlockState Stmt
evalStmt stmt = case stmt of
   (AssignStmt a) -> AssignStmt <$>  checkAssign a
   (If expr stmt') -> checkIf expr stmt'
   (IfElse expr stmt1 stmt2) -> checkIfElse expr stmt1 stmt2
   (Seq blockStmt) -> Seq <$> checkBlock blockStmt M.empty
   (BStmt blockStmt) -> BStmt <$> checkBlock blockStmt M.empty
   (Return expr) -> checkReturn expr
   (ForLoop ident expr1 expr2 expr3 stmts) -> checkForLoop ident expr1 expr2 expr3 stmts
   (MethodStmt method) -> MethodStmt <$> checkMethodCall method
   _ -> lift $ Left "Not implemented"


-- |Type Check Assignment Statement
checkAssign :: Assign -> BlockState Assign
checkAssign (Assign gType ident expr) = do
    ftable <- use funcDefs
    oldVtable <- use prevVars
    vTable <- use curVars
    cTable <- use constVars
    ptrs <- use pointers
    let scopeVars = vTable `M.union` oldVtable -- Gives all visible identifiers
    reducedExpr <- lift $ reduceExpr scopeVars $ injectPtrs ptrs $ injectConstants cTable expr
    if ident `M.notMember` vTable then do
        exprType <- lift $ getTypeExpr scopeVars ftable reducedExpr
       
        if gType == exprType then do
            -- Update Var table with new variable
            assign curVars   $ M.insert ident gType vTable
            -- Update Const Table
            assign constVars $ case expr of 
                            (ExpLit l) -> M.insert ident l cTable
                            _ -> M.delete ident cTable
            return $ Assign gType ident reducedExpr
        
        -- If expression is a method call, we implicitly lift the type
        -- of the entire expression into the GPRM::Kernel
        else if isMethodCall reducedExpr then do
            let gType' = castToKernel gType 
            assign curVars $ M.insert ident gType' vTable
            return $ Assign gType' ident reducedExpr

        else typeMismatch gType exprType
    else redefine
 where
    redefine = lift $ Left $ "Error, cannot redefine " ++ show ident ++ " in current scope" 
    typeMismatch l r = lift $ Left $ show ident ++ " declared as type " ++ show l ++
                            "but rhs evaluates to type " ++ show r

    isMethodCall exp = case exp of
        (ExpMethodCall (MethodCall{})) -> True
        _ -> False

-- |Type Check If Statement
checkIf :: Expr -> Stmt -> BlockState Stmt
checkIf expr stmt = do
    fTable <- use funcDefs
    cTable <- use constVars
    ptrs <- use pointers
    scopeVars <- M.union <$> use curVars <*> use prevVars 
    exprType <- lift $ getTypeExpr scopeVars fTable expr
    reducedExpr <- lift $ reduceExpr scopeVars $ injectPtrs ptrs $ injectConstants cTable expr
    if exprType == boolType notKernel then do
        reducedStmt <- evalStmt stmt
        return $ If reducedExpr reducedStmt
    else lift $ Left $ "Expression within if is expected to be a bool " ++
        " but it evaluates to a " ++ show exprType


-- |Type Check If - Else Statement
checkIfElse :: Expr -> Stmt -> Stmt -> BlockState Stmt
checkIfElse expr thenStmt elseStmt = do
    reducedIf <- checkIf expr thenStmt
    reducedElse <- evalStmt elseStmt
    case reducedIf of
        (If expr1 stmt) ->
            return $ IfElse expr1 stmt reducedElse
        _ -> lift $ Left "Compiler error in checkIfElse"


-- | Type check for loop
checkForLoop :: Ident -> Expr -> Expr -> Expr -> BlockStmt -> BlockState Stmt
checkForLoop ident startExpr stopExpr stepExpr blockStmt = do
    cTable <- use constVars
    fTable <- use funcDefs
    ptrs <- use pointers
    scopeVars <- M.union <$> use curVars <*> use prevVars 

    startExpr' <- lift $ reduceExpr scopeVars $ injectPtrs ptrs $ injectConstants cTable startExpr
    stopExpr'  <- lift $ reduceExpr scopeVars $ injectPtrs ptrs $ injectConstants cTable stopExpr
    stepExpr'  <- lift $ reduceExpr scopeVars $ injectPtrs ptrs $ injectConstants cTable stepExpr

    -- Check all expressions are constant (for loops are static)
    -- Check types of each expression are all integers
    -- Then type check the for block
    let exprs = [startExpr', stopExpr', stepExpr']
    mapM_ checkConstantExpr exprs
    types <- lift $ mapM (getTypeExpr scopeVars fTable) exprs
    mapM_ (checkType $ intType notKernel) types

    reducedBlock <- checkBlock blockStmt (M.singleton ident (intType notKernel))
    return $ ForLoop ident startExpr' stopExpr' stepExpr' reducedBlock
     

-- | Type check inner block, add to current list of inner blocks
checkBlock :: BlockStmt -> VarTable -> BlockState BlockStmt
checkBlock (BlockStmt stmts) innerTable = do
    fName <- use currentFun
    fTable <- use funcDefs
    cTable <- use constVars
    ptrs <- use pointers
    scopeVars <- M.union <$> use curVars <*> use prevVars 
    
    -- Create and type check new inner block, and add to current
    -- list of inner blocks if successful
    let newBlock = CodeBlock fName fTable scopeVars innerTable ptrs cTable   
    lift $ runBlockCheck stmts newBlock         


-- | Type check return stmt
checkReturn :: Expr -> BlockState Stmt
checkReturn expr = do
    fName <- use currentFun
    fTable <- use funcDefs
    cTable <- use constVars
    ptrs <- use pointers
    scopeVars <- M.union <$> use curVars <*> use prevVars 
    reducedExpr <- lift $ reduceExpr scopeVars $ injectPtrs ptrs $ injectConstants cTable expr

    let notFound = "Error, function not found " ++ show fName

    (retType, _) <- lift $ note notFound $ M.lookup fName fTable
    exprType <- lift $ getTypeExpr scopeVars fTable expr

    if retType == exprType then
        return $ Return reducedExpr
    else
        lift $ Left $ "The return type of function " ++ show fName ++
            "is " ++ show retType ++ "but return expression evaluates to" ++
            "type " ++ show exprType


-- | TODO Type check method call
checkMethodCall :: MethodCall -> BlockState MethodCall
checkMethodCall = return


-- | Check that an expression is Constant
checkConstantExpr :: Expr -> GenericBlockState a ()
checkConstantExpr expr = case expr of
    (ExpLit _) -> modify id
    _ -> lift $ Left "expected constant expression"


-- | Checks that 2 given types match
checkType :: Type -> Type -> GenericBlockState a ()
checkType expected actual = 
    if expected == actual then modify id else 
        lift $ Left $ show expected ++ " but expression evaluated to " ++ show actual


-- | Obtain Type of Expression, returns error message
-- | if types arn't consistent, or identifiers arn't in scope
-- | TODO seperate this huge function
getTypeExpr :: VarTable -> FunTable -> Expr -> Either String Type
getTypeExpr vtable ftable expr = case expr of
    (ExpBinOp b e1 e2) -> getTypeBinOp vtable ftable b e1 e2
    (ExpUnaryOp u e) -> getTypeUnOp vtable ftable u e
    (ExpFunCall (FunCall s exps)) -> do
        argTypes <- mapM (getTypeExpr vtable ftable) exps
        (retT, ts) <- note (notFound s) (M.lookup s ftable)
        if length argTypes /= length ts
            then Left $ "Function " ++ show s ++  " expects " ++ show (length ts) ++
                      " arguments but was given " ++ show (length argTypes)
            else if argTypes /= ts 
                then Left "Arguments don't evaluate to given types"
                else Right retT

    (ExpMethodCall _) -> return $ NormalType inKernel "Object"
    (ExpIdent i) -> note (notFound i) (M.lookup i vtable) 

    (ExpPointer (Pointer ident@(Ident i) n)) -> 
        PointerType <$> note (notFound ident) (M.lookup catIdent vtable)
          where catIdent = Ident (show n ++ i)

    (ExpLit l) -> return $ flip id notKernel $ case l of
                Str _ -> strType
                Ch _ -> chType
                Number (Left _) -> intType
                Number (Right _) -> doubleType 
                Bl _ -> boolType

 where notFound (Ident i) = "Identifier " ++ i ++ " not declared in scope"
        

-- Get Type of a Binary Expression
getTypeBinOp :: VarTable -> FunTable -> BinOps -> Expr -> Expr -> Either String Type
getTypeBinOp vtable ftable bop e1 e2  = do
    leftType <- getTypeExpr vtable ftable e1
    rightType <- getTypeExpr vtable ftable e2
    -- If one of the expressions is in the kernel, cast the other
    -- one into the kernel
    let leftType' = if isInKernel rightType then castToKernel leftType else leftType
        rightType' = if isInKernel leftType then castToKernel rightType else rightType
    if isPointer leftType' || isPointer rightType' then
        getPointerTypeBin bop leftType' rightType'
    else getNormalTypeBin bop leftType' rightType'
        

-- Get Type of binary expression involving no pointers
getNormalTypeBin :: BinOps -> Type -> Type -> Either String Type
getNormalTypeBin bop leftType rightType          
   | bop `elem` numNumNumOp = 
       if leftType /= rightType then 
           Left "Both expressions expected to be the same type"
       else if leftType == intType' then return intType'
       else if leftType == doubleType' then return doubleType'
       else Left "Expected integer or double type"

   | bop `elem` intIntIntOp = 
       if (leftType, rightType) == (intType', intType')
           then return intType' 
           else Left "Expected integer values on both sides"              

   | bop `elem` compareOp = 
       if (leftType, rightType) `elem` [(intType', intType'), (doubleType', doubleType')] 
           then return boolType'
           else Left "Expected numeric values of the same type"      

   | bop `elem` boolOp = 
       if (leftType, rightType) == (boolType', boolType')
           then return boolType'
           else Left "Expected boolean values"    
             
    | otherwise = Left "Compiler error during obtaining type of binary expression"
  where 
     numNumNumOp = [Add, Sub, Mul, Div]        
     intIntIntOp = [Mod, BAnd, BOr, BXor, ShiftL, ShiftR]
     compareOp = [LessEq, Less, Equals, Greater, GreaterEq]
     boolOp = [And, Or]
     intType' = intType kernel
     boolType' = boolType kernel
     doubleType' = doubleType kernel
     kernel = isInKernel leftType

-- Get type of binary expression involving pointers 
getPointerTypeBin :: BinOps -> Type -> Type -> Either String Type
getPointerTypeBin bop leftType rightType
    | bop == Add = 
        if isPointer leftType && rightType == intType' then
            return leftType    
        else if isPointer rightType && leftType == intType' then
            return rightType
        else Left "Can only add Pointers to Integers"

    | bop == Sub =
        if isPointer leftType && rightType == intType' then
        return leftType
        else Left "expected pointer type on lhs and integer type on rhs for pointer subtraction"                
    | bop `elem` [Equals, NEquals] = case (leftType, rightType) of
        ((PointerType a, PointerType b)) -> 
            if a == b then 
                return boolType'
            else Left $ "Expected pointer types to be equal, left points to " ++ show a ++
                  ". Right points to " ++ show b ++ "."
        _ -> Left "Cannot perform an equality comparison of pointer and non pointer types"

    | otherwise =  Left $ "operation " ++ show bop ++ " not defined for pointer types"
  where
     intType' = intType kernel
     boolType' = boolType kernel
     kernel = isInKernel leftType


--Get type of unary expression
getTypeUnOp :: VarTable -> FunTable -> UnaryOps -> Expr -> Either String Type
getTypeUnOp vtable ftable operation expr 
    | operation == Not || operation == Neg = getTypeExpr vtable ftable expr >>= 
        \t -> case t of 
            (NormalType kernel "int") -> return $ NormalType kernel "int"
            e -> Left $ "Expected integer expression, but found" ++ show e

    | operation == BNot = getTypeExpr vtable ftable expr >>=
        \t -> case t of
            (NormalType kernel "bool") -> return $ NormalType kernel "bool"
            e -> Left $ "Expected boolean expression, but found " ++ show e

    | otherwise = Left "Compiler error during obtaining type of unary expression"


-- Replace all constant identifiers with their
-- constant value
injectConstants :: ConstVarTable ->  Expr -> Expr
injectConstants ctable expr = case expr of
    (ExpBinOp b e1 e2) -> ExpBinOp b (injectConstants ctable e1) (injectConstants ctable e2)
    (ExpUnaryOp u e) -> ExpUnaryOp u (injectConstants ctable e)
    (ExpFunCall (FunCall s exps)) -> ExpFunCall (FunCall s (map (injectConstants ctable) exps))
    (ExpMethodCall (MethodCall obj method args)) -> 
        ExpMethodCall (MethodCall obj method (map (injectConstants ctable) args))
    (ExpIdent i) -> case M.lookup i ctable of
                                Just l ->   ExpLit l
                                Nothing ->  ExpIdent i
    (ExpLit l) -> ExpLit l
    (ExpPointer p) -> ExpPointer p


injectPtrs :: PtrTable -> Expr -> Expr
injectPtrs ptrs expr = case expr of    
    (ExpBinOp b e1 e2) -> ExpBinOp b (injectPtrs ptrs e1) (injectPtrs ptrs e2)
    (ExpUnaryOp u e) -> ExpUnaryOp u (injectPtrs ptrs e)
    (ExpFunCall (FunCall s exps)) -> ExpFunCall (FunCall s (map (injectPtrs ptrs) exps))
    (ExpMethodCall (MethodCall obj method args)) -> 
        ExpMethodCall (MethodCall obj method (map (injectPtrs ptrs) args))
    (ExpIdent i) -> case M.lookup i ptrs of
                                Just p ->   ExpPointer p
                                Nothing ->  ExpIdent i
    (ExpLit l) -> ExpLit l
    (ExpPointer p) -> ExpPointer p


-- | Attempts to reduce an expression as much as possible
-- | Returns an error string if evaluated expression 
-- | is invalid or an identifier is not present in the given table
-- | otherwise returns the reduced expression
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
                                    then Right i
                                    else Left $ notFound i

    (ExpLit l) -> return (ExpLit l)
    (ExpPointer p) -> return (ExpPointer p)
 where notFound (Ident i) = "Identifier " ++ i ++ " not declared in scope"

-- | Attempts to evaluate a constant binary expression, checks the types as well
evaluateBinExpr :: BinOps -> Expr -> Expr -> Either String Expr
-- Binary operations with literals
evaluateBinExpr b (ExpLit l1) (ExpLit l2) = binOpTable b l1 l2
-- Binary Operation with pointers
evaluateBinExpr Add (ExpPointer (Pointer ident n)) (ExpLit (Number (Left i))) =
    Right $ ExpPointer $ Pointer ident (n + i)
evaluateBinExpr Add e (ExpPointer p) = evaluateBinExpr Add (ExpPointer p) e
evaluateBinExpr Sub (ExpPointer (Pointer ident n)) (ExpLit (Number (Left i))) = 
   Right $ ExpPointer $ Pointer ident (n - i)
evaluateBinExpr Equals (ExpPointer p1) (ExpPointer p2) = 
   Right $  ExpLit $ Bl (p1 == p2)
evaluateBinExpr  b e1 e2 = return $ ExpBinOp b e1 e2


-- | Obtain binary operation to use with literal values
binOpTable :: BinOps -> Literal -> Literal -> Either String Expr
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
evaluateUnExpr unOp (ExpLit l) = unOpTable unOp l
evaluateUnExpr _ e = Right e

-- | Function table of Unary operations on literals
unOpTable u = case u of
    Not -> performUnNotOp
    Neg -> performUnNegOp
    BNot -> performUnBNotOp

-- | Perform Boolean NOT operation on literal value
performUnNotOp ::  Literal -> Either String Expr
performUnNotOp (Bl b1) = Right $ ExpLit $ Bl $ not b1
performUnNotOp _ = Left "Error expected boolean value"

-- | Perform Negation operation on literal value
performUnNegOp :: Literal -> Either String Expr
performUnNegOp (Number (Left i)) =  Right $ ExpLit $ Number $ Left  $ negate i
performUnNegOp (Number (Right i)) = Right  $ ExpLit $ Number $ Right $ negate i
performUnNegOp _ = Left "Error expected numeric type"

-- | Perform Bitwise NOT operation on literal value
performUnBNotOp :: Literal -> Either String Expr
performUnBNotOp (Number (Left i)) = Right $ ExpLit $ Number $ Left $ complement i
performUnBNotOp _ = Left "Error expected integer value"
