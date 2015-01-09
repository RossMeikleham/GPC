{-# LANGUAGE TemplateHaskell #-}
{- Check types and scope of identifiers -}
module GPC.TypeScopeChecker(
    getTypeExpr, runTypeChecker) where


import           Control.Applicative hiding (empty, many, optional, (<|>))
import           Control.Error.Util
import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.Bits
import           Data.Tuple
import qualified Data.Map as M
import           GPC.AST

type VarTable = M.Map Ident Type
type ConstVarTable = M.Map Ident Literal
type FunTable = M.Map Ident (Type, [Type])
type ObjectTable = M.Map Ident Objects

boolType = flip NormalType "bool"
intType = flip NormalType "int"
strType = flip NormalType "string"
chType = flip NormalType "char"
doubleType = flip NormalType "double"
objectType = NormalType True "object"
arrayObjType = NormalType True "objArray"

-- Upcast a type to a Kernel type, if
-- the given type is already a Kernel type then
-- returns the given type
castToKernel :: Type -> Type
--castToKernel (PointerType t) = PointerType $ castToKernel t
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
    _tlFuncDefs      :: FunTable, -- ^ Function Definitions
    _tlVarTypes        :: VarTable,  -- ^ Top Level Constant variable types
    _objects         :: ObjectTable   
} deriving (Show)


data CodeBlock = CodeBlock {
    _currentFun :: Ident, -- ^ Name of Function block is in
    _funcDefs   :: FunTable, -- ^ Function names and return/argument types
    _prevVars   :: VarTable, -- ^ Identifiers visible in current scope with types
    _curVars    :: VarTable  -- ^ Identifiers declared in current scope
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
 where initialBlock = MainBlock M.empty M.empty M.empty


-- | Type Check all top level statements
evalTLStmts :: [TopLevel] -> CodeState [TopLevel]
evalTLStmts = mapM evalTLStmt


-- | Type check a given top level statement
evalTLStmt :: TopLevel -> CodeState TopLevel
evalTLStmt tl = case tl of
    (TLAssign a) -> TLAssign <$> evalTLAssign a
    (Func gType ident args stmts) -> evalFunc gType ident args stmts
    (TLObjs objs) -> TLObjs <$> evalObjs objs
    (TLConstructObjs cObjs) -> TLConstructObjs <$> evalConstruct cObjs

-- | Type check object initializations
evalConstruct :: ConstructObjs -> CodeState ConstructObjs
evalConstruct (ConstructObjs ns var exprs) = do
    tVars <- use tlVarTypes
    objs <- use objects
    _ <- lift $ mapM (getTypeExpr tVars M.empty) exprs

    case var of
        (VarIdent ident) -> do
            v <- checkNameSpace ident objs
            case v of
                (VarIdent _) -> return $ ConstructObjs ns var exprs
                (VarArrayElem i _) -> lift $ Left $ show i ++ "is declared as a " ++
                    "single object, not an array"

        (VarArrayElem ident expr) -> do -- Check indexed expression
            v <- checkNameSpace ident objs
      
            -- Check index is an integer value
            exprType <- lift $ getTypeExpr tVars M.empty expr
            checkType (intType notKernel) exprType

            case v of 
                (VarIdent i) -> lift $ Left $ show i ++ "is declared as an array"
                    ++ "of objects, expecting assignment to a single element"
                (VarArrayElem i expr') -> do
                    -- Check indexing is in bounds

                    exprType' <- lift $ getTypeExpr tVars M.empty expr'
                    checkType (intType notKernel) exprType'
                    return $ ConstructObjs ns var exprs
    
  where             
    notFound i = "Error, object not found " ++ show i
    checkNameSpace i objs = do {
    (Objects objNs oVar) <- lift $ note (notFound i) (M.lookup i objs);
    if objNs /= init ns then
         lift $ Left $ "No Object(s) declared with namespace " ++
            show (init ns) ++ "perhaps you meant " ++ show objNs ++ "?"
    else if last ns /= last (init ns) then
         lift $ Left $ "Expected object constructor " ++
            show (init ns ++ [last ns]) ++ " but found " ++ show ns
    else return oVar
    }



-- | Type check object declarations
evalObjs :: Objects -> CodeState Objects
evalObjs objs@(Objects _ var) = do
    
    tVars <- use tlVarTypes
    case var of
        -- Single Object, check identifier isn't already in scope
        (VarIdent ident) -> do
            checkMultipleInstance ident tVars
            objects %= (M.insert ident objs) 
            assign tlVarTypes $ M.insert ident (NormalType inKernel "object") tVars
            return objs

        -- Static Array of Objects, check type of array size
        (VarArrayElem ident expr) -> do
            checkMultipleInstance ident tVars
            exprType <- lift $ getTypeExpr tVars M.empty expr
            checkType (intType notKernel) exprType
            objects %= (M.insert ident objs) 
            assign tlVarTypes $ M.insert ident (NormalType inKernel "objArray") tVars
            return $ objs {objVar = VarArrayElem ident expr}


-- | Type Check top level assignment
evalTLAssign :: Assign -> CodeState Assign
evalTLAssign (Assign typeG ident expr) = do
    tVars <- use tlVarTypes
    exprType <- lift $ getTypeExpr tVars M.empty expr

    -- Check Types match and Variable is Single instance
    checkType exprType typeG
    checkMultipleInstance ident tVars
    assign tlVarTypes $ M.insert ident typeG tVars
    return $ Assign typeG ident expr


-- | Type check Function
evalFunc :: Type -> Ident -> [(Type, Ident)] -> BlockStmt -> CodeState TopLevel
evalFunc typeG ident args (BlockStmt stmts) = do
    fTable <- use tlFuncDefs
    varTypes <- use tlVarTypes

    -- Check function isn't already defined
    if ident `M.notMember` fTable
        -- Check argument identifiers only occur once each
        then if hasDuplicate (map snd args) then lift $ Left $ 
                  "Function " ++ show ident ++ "contains duplicate argument names"

        else do
            -- Create a new variable scope based on the identifiers
            -- in the function definition
            let args' = M.fromList $ map swap args

            -- Add current function to the function scope
            let newFTable = M.insert ident (typeG, map fst args) fTable

            -- Type Check function
            let newBlock = CodeBlock ident newFTable varTypes args'
            funBlock <- lift $ runBlockCheck stmts newBlock

            -- Update Global function scope, and return reduced function
            -- AST
            assign tlFuncDefs newFTable
            return $ Func typeG ident args funBlock

    else lift $ Left $ "Function " ++ show ident ++ "occurs more than once"
  where hasDuplicate (x:xs) = x `elem` xs || hasDuplicate xs
        hasDuplicate _ = False 

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
   (FunCallStmt fc) -> FunCallStmt <$> checkFunCall fc


-- | Type check Function Call args
checkFunCall :: FunCall -> BlockState FunCall
checkFunCall f@(FunCall name args) = do    
    fTable <- use funcDefs
    oldVtable <- use prevVars
    vTable <- use curVars
    let scopeVars = vTable `M.union` oldVtable
    _ <- lift $ getTypeExpr scopeVars fTable (ExpFunCall f)
    return $ FunCall name args


-- |Type Check Assignment Statement
checkAssign :: Assign -> BlockState Assign
checkAssign (Assign gType ident expr) = do
    ftable <- use funcDefs
    oldVtable <- use prevVars
    vTable <- use curVars
    let scopeVars = vTable `M.union` oldVtable -- Gives all visible identifiers
    
    -- Check new identifier hasn't already been defined in the current
    -- scope
    redefineCheck vTable

    -- If expression is a method call, we implicitly lift the type
    -- of the entire expression into the GPRM::Kernel
    if isMethodCall expr then do
        let gType' = castToKernel gType
        assign curVars $ M.insert ident gType' vTable
        return $ Assign gType' ident expr

    else do 
        exprType <- lift $ getTypeExpr scopeVars ftable expr
        checkType gType exprType
        -- Update Var table with new variable
        assign curVars   $ M.insert ident gType vTable
        return $ Assign gType ident expr

 where
    redefineCheck vTable = when (ident `M.member` vTable) $
        lift $ Left $ "Error, cannot redefine " ++ show ident ++ " in current scope"

    isMethodCall e = case e of
        (ExpMethodCall (MethodCall{})) -> True
        _ -> False


-- |Type Check If Statement
checkIf :: Expr -> Stmt -> BlockState Stmt
checkIf expr stmt = do
    fTable <- use funcDefs
    scopeVars <- M.union <$> use curVars <*> use prevVars
    exprType <- lift $ getTypeExpr scopeVars fTable expr
    checkType (boolType notKernel) exprType
    return stmt

-- |Type Check If - Else Statement
checkIfElse :: Expr -> Stmt -> Stmt -> BlockState Stmt
checkIfElse expr thenStmt elseStmt = do
    _ <- checkIf expr thenStmt
    return $ IfElse expr thenStmt elseStmt


-- | Type check for loop
checkForLoop :: Ident -> Expr -> Expr -> Expr -> BlockStmt -> BlockState Stmt
checkForLoop ident startExpr stopExpr stepExpr blockStmt = do
    fTable <- use funcDefs
    scopeVars <- M.union <$> use curVars <*> use prevVars

    -- Check types of each expression are all integers
    -- Then type check the for block
    let exprs = [startExpr, stopExpr, stepExpr]
    types <- lift $ mapM (getTypeExpr scopeVars fTable) exprs
    mapM_ (checkType $ intType notKernel) types

    block <- checkBlock blockStmt (M.singleton ident (intType notKernel))
    return $ ForLoop ident startExpr stopExpr stepExpr block


-- | Type check inner block, add to current list of inner blocks
checkBlock :: BlockStmt -> VarTable -> BlockState BlockStmt
checkBlock (BlockStmt stmts) innerTable = do
    fName <- use currentFun
    fTable <- use funcDefs
    scopeVars <- M.union <$> use curVars <*> use prevVars

    -- Create and type check new inner block, and add to current
    -- list of inner blocks if successful
    let newBlock = CodeBlock fName fTable scopeVars innerTable
    lift $ runBlockCheck stmts newBlock


-- | Type check return stmt
checkReturn :: Expr -> BlockState Stmt
checkReturn expr = do
    fName <- use currentFun
    fTable <- use funcDefs
    scopeVars <- M.union <$> use curVars <*> use prevVars

    let notFound = "Error, function not found " ++ show fName

    (retType, _) <- lift $ note notFound $ M.lookup fName fTable
    exprType <- lift $ getTypeExpr scopeVars fTable expr

    checkType retType exprType
    return $ Return expr


checkMethodCall :: MethodCall -> BlockState MethodCall
checkMethodCall (MethodCall var method args) = do
    vTable <- M.union <$> use curVars <*> use prevVars
    fTable <- use funcDefs
    scopeVars <- M.union <$> use curVars <*> use prevVars
    
    case var of
        (VarIdent i) -> do
            gType <- lift $ findType i vTable
            checkType objectType gType
            return $ MethodCall var method args           
            
        -- Check we're accessing an element of an array of objects,
        -- and that the element is an integer type
        (VarArrayElem i el) -> do
            gType <- lift $ findType i vTable
            elemType <- lift $ getTypeExpr scopeVars fTable el
            checkType (intType False) elemType
            checkType  arrayObjType gType
            return $ MethodCall var method args


  where findType i vTable = note (notFound i) (M.lookup i vTable)
        notFound  obj = "Error, object not found " ++ show obj


-- | Checks that 2 given types match
checkType :: Type -> Type -> GenericBlockState a ()
checkType expected actual =
    if expected == actual then modify id else
        lift $ Left $ show expected ++ " but expression evaluated to " ++ show actual


checkType' :: Type -> Type -> Either String ()
checkType' expected actual =
    if expected == actual then Right () else
        Left $ show expected ++ " but expression evaluated to " ++ show actual


-- | Given an identity and var table, if the identity is already
-- part ofr the variable table, then returns a multiple instance error
checkMultipleInstance :: Ident -> VarTable -> GenericBlockState a ()
checkMultipleInstance ident@(Ident i) vTable = when (ident `M.member` vTable) $ 
    lift $ Left $ i ++ " has already been defined in scope, cannot redefine it"


-- | Obtain Type of Expression, returns error message
-- | if types arn't consistent, or identifiers arn't in scope
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

    (ExpMethodCall (MethodCall var _ args)) -> do
        _ <- mapM (getTypeExpr vtable ftable) args

        case var of
            (VarIdent i) -> do
                objType <- note (notFound i) (M.lookup i vtable)
                checkType' objType objectType
                return $ NormalType inKernel "object"
            
            (VarArrayElem i el) -> do
                objType <- note (notFound i) (M.lookup i vtable)
                elemType <- getTypeExpr vtable ftable el
                checkType' (intType False) elemType
                checkType' objType arrayObjType
                return $ NormalType inKernel "object"

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

   | bop `elem` eqOp = 
        if leftType == rightType 
            then return boolType'
            else Left "Expected equality of same types"

   | otherwise = Left "Compiler error during obtaining type of binary expression"
  where
     numNumNumOp = [Add, Sub, Mul, Div]
     intIntIntOp = [Mod, BAnd, BOr, BXor, ShiftL, ShiftR]
     compareOp = [LessEq, Less, Greater, GreaterEq]
     eqOp = [Equals, NEquals]
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
    | operation == BNot = getTypeExpr vtable ftable expr >>=
        \t -> case t of
            (NormalType kernel "int") -> return $ NormalType kernel "int"
            e -> Left $ "Expected integer expression, but found" ++ show e

    | operation == Neg = getTypeExpr vtable ftable expr >>=
        \t -> case t of
            (NormalType kernel "int") -> return $ NormalType kernel "int"
            (NormalType kernel "double") -> return $ NormalType kernel "double"
            e -> Left $ "Expected integer expression, but found" ++ show e

    | operation == Not = getTypeExpr vtable ftable expr >>=
        \t -> case t of
            (NormalType kernel "bool") -> return $ NormalType kernel "bool"
            e -> Left $ "Expected boolean expression, but found " ++ show e

    | otherwise = Left "Compiler error during obtaining type of unary expression"
