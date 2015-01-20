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
import qualified GPC.Interpreter as I

type VarTable = M.Map (Ident SrcPos) (Type SrcPos)
type ConstVarTable = M.Map (Ident SrcPos) (Literal SrcPos)
type FunTable = M.Map (Ident SrcPos) (Type SrcPos, [Type SrcPos])
type ObjectTable = M.Map (Ident SrcPos) (Objects SrcPos)

boolType b = NormalType () b "bool"
boolTypePos b s = NormalType s b "bool"
intType b = NormalType () b "int"
intTypePos b s = NormalType s b "int"
strType b = NormalType () b "string"
strTypePos b s = NormalType s b "string"
chType b = NormalType () b "char"
chTypePos b s = NormalType s b "char"
doubleType b = NormalType () b "double"
doubleTypePos b s = NormalType s b "double"
objectType = NormalType () True "object"
arrayObjType = NormalType () True "objArray"

-- Upcast a type to a Kernel type, if
-- the given type is already a Kernel type then
-- returns the given type
castToKernel :: Type a -> Type a
castToKernel (PointerType t) = PointerType $ castToKernel t
castToKernel (NormalType a _ n) = NormalType a True n

isInKernel :: Type a -> Bool
isInKernel (PointerType t) = isInKernel t
isInKernel (NormalType _ k _) = k

notKernel = False
inKernel = True

isPointer :: Type a -> Bool
isPointer (PointerType _) = True
isPointer _ = False

data MainBlock = MainBlock {
    _tlFuncDefs      :: FunTable, -- ^ Function Definitions
    _tlVarTypes        :: VarTable,  -- ^ Top Level Constant variable types
    _objects         :: ObjectTable   
} deriving (Show)


data CodeBlock = CodeBlock {
    _currentFun :: (Ident SrcPos), -- ^ Name of Function block is in
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
runTypeChecker :: (Program SrcPos) -> Either String [I.TLStmt] 
runTypeChecker (Program tls) = case runStateT (evalTLStmts tls) initialBlock of
 Left s -> Left s
 (Right (tl, _)) -> Right $ Program tl
 where initialBlock = MainBlock M.empty M.empty M.empty


-- | Type Check all top level statements
evalTLStmts :: [TopLevel SrcPos] -> CodeState [I.TLStmt]
evalTLStmts = mapM evalTLStmt


-- | Type check a given top level statement
evalTLStmt :: TopLevel SrcPos -> CodeState I.TLStmt
evalTLStmt tl = case tl of
    (TLAssign a) -> TLAssign <$> evalTLAssign a
    (Func gType ident args stmts) -> evalFunc gType ident args stmts
    (TLObjs objs) -> TLObjs <$> evalObjs objs
    (TLConstructObjs cObjs) -> TLConstructObjs <$> evalConstruct cObjs

-- | Type check object initializations
evalConstruct :: (ConstructObjs SrcPos) -> CodeState I.TLStmt
evalConstruct (ConstructObjs ns var exprs) = do
    tVars <- use tlVarTypes
    objs <- use objects
    exprs' <- lift $ mapM (getTypeExpr tVars M.empty) exprs

    case var of
        (VarIdent ident) -> do
            v <- checkNameSpace ident objs
            case v of
                (VarIdent _) -> return $ I.TLConstruct (I.Ident "test") exprs'
                (VarArrayElem i _) -> lift $ Left $ errorIdent i ++  show i ++ 
                    "is declared as a single object, not an array"

        (VarArrayElem ident@(Ident sp s) expr) -> do -- Check indexed expression
            v <- checkNameSpace ident objs
      
            -- Check index is an integer value
            exprType <- lift $ getTypeExpr tVars M.empty expr
            checkType (intTypePos notKernel sp) exprType

            case v of 
                (VarIdent i) -> lift $ Left $ errorIdent i ++ show i ++ "is declared as an array"
                    ++ "of objects, expecting assignment to a single element"
                (VarArrayElem i@(Ident sp' s') expr') -> do
                   
                    exprType' <- lift $ getTypeExpr tVars M.empty expr'
                    checkType (intTypePos notKernel sp') exprType'
                    return $ I.TLConstruct (I.Ident "test") exprs'
    
  where             
    notFound i =  errorIdent i ++ "object not found " ++ show i
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
evalObjs :: Objects SrcPos -> CodeState I.TLStmt
evalObjs objs@(Objects _ var) = do
    
    tVars <- use tlVarTypes
    case var of
        -- Single Object, check identifier isn't already in scope
        (VarIdent ident@(Ident sp s)) -> do
            checkMultipleInstance ident tVars
            objects %= (M.insert ident objs) 
            assign tlVarTypes $ M.insert ident (NormalType sp inKernel "object") tVars
            return objs

        -- Static Array of Objects, check type of array size
        (VarArrayElem ident@(Ident sp s) expr) -> do
            checkMultipleInstance ident tVars
            exprType <- lift $ getTypeExpr tVars M.empty expr
            checkType (intTypePos notKernel sp) exprType
            objects %= (M.insert ident objs) 
            assign tlVarTypes $ M.insert ident (NormalType sp inKernel "objArray") tVars
            return $ objs {objVar = VarArrayElem ident expr}


-- | Type Check top level assignment
evalTLAssign :: (Assign SrcPos) -> CodeState (Assign SrcPos)
evalTLAssign (Assign typeG ident expr) = do
    tVars <- use tlVarTypes
    exprType <- lift $ getTypeExpr tVars M.empty expr

    -- Check Types match and Variable is Single instance
    checkType typeG exprType
    checkMultipleInstance ident tVars
    assign tlVarTypes $ M.insert ident typeG tVars
    return $ Assign typeG ident expr


-- | Type check Function
evalFunc :: Type SrcPos -> Ident SrcPos -> [(Type SrcPos, Ident SrcPos)] 
                        -> BlockStmt SrcPos -> CodeState (TopLevel SrcPos)
evalFunc typeG ident args (BlockStmt stmts) = do
    fTable <- use tlFuncDefs
    varTypes <- use tlVarTypes

    -- Check function isn't already defined
    if ident `M.notMember` fTable then
        -- Check argument identifiers only occur once each
        case hasDuplicate (map snd args) of
            Just argId -> lift $ Left $ errorIdent argId ++
                  "Function " ++ show ident ++ "contains duplicate argument names " ++ show argId
            Nothing -> do

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

    else lift $ Left $ errorIdent ident ++  "Function " ++ show ident ++ "occurs more than once"
  where hasDuplicate (x:xs) = if x `elem` xs then Just x else hasDuplicate xs
        hasDuplicate _ = Nothing

-- | Run Type Checker on new code block
runBlockCheck :: [Stmt SrcPos] -> CodeBlock -> Either String (BlockStmt SrcPos)
runBlockCheck stmts cb =  case runStateT (evalStmts stmts) cb of
    Left s -> Left s
    (Right (stmts', _)) -> Right $ BlockStmt stmts'


-- | Type Check all statements in the current scope
evalStmts :: [Stmt SrcPos] -> BlockState [Stmt SrcPos]
evalStmts = mapM evalStmt


-- | Type check given statement
evalStmt :: Stmt SrcPos -> BlockState (Stmt SrcPos)
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
checkFunCall :: (FunCall SrcPos) -> BlockState (FunCall SrcPos)
checkFunCall f@(FunCall name args) = do    
    fTable <- use funcDefs
    oldVtable <- use prevVars
    vTable <- use curVars
    let scopeVars = vTable `M.union` oldVtable
    _ <- lift $ getTypeExpr scopeVars fTable (ExpFunCall f)
    return $ FunCall name args


-- |Type Check Assignment Statement
checkAssign :: Assign SrcPos -> BlockState (Assign SrcPos)
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
        lift $ Left $ errorIdent ident ++ "Error, cannot redefine " ++ show ident ++ " in current scope"

    isMethodCall e = case e of
        (ExpMethodCall (MethodCall{})) -> True
        _ -> False


-- |Type Check If Statement
checkIf :: Expr SrcPos -> Stmt SrcPos -> BlockState (Stmt SrcPos)
checkIf expr stmt = do
    fTable <- use funcDefs
    scopeVars <- M.union <$> use curVars <*> use prevVars
    exprType <- lift $ getTypeExpr scopeVars fTable expr
    checkType exprType (boolType notKernel)
    return stmt

-- |Type Check If - Else Statement
checkIfElse :: Expr SrcPos -> Stmt SrcPos -> Stmt SrcPos -> BlockState (Stmt SrcPos)
checkIfElse expr thenStmt elseStmt = do
    _ <- checkIf expr thenStmt
    return $ IfElse expr thenStmt elseStmt


-- | Type check for loop
checkForLoop :: Ident SrcPos -> Expr SrcPos -> Expr SrcPos 
                             -> Expr SrcPos -> BlockStmt SrcPos 
                             -> BlockState (Stmt SrcPos)
checkForLoop ident@(Ident sp s) startExpr stopExpr stepExpr blockStmt = do
    fTable <- use funcDefs
    scopeVars <- M.union <$> use curVars <*> use prevVars

    -- Check types of each expression are all integers
    -- Then type check the for block
    let exprs = [startExpr, stopExpr, stepExpr]
    types <- lift $ mapM (getTypeExpr scopeVars fTable) exprs
    mapM_ (flip checkType $ intType notKernel) types

    block <- checkBlock blockStmt (M.singleton ident (intTypePos notKernel sp))
    return $ ForLoop ident startExpr stopExpr stepExpr block


-- | Type check inner block, add to current list of inner blocks
checkBlock :: BlockStmt SrcPos -> VarTable -> BlockState (BlockStmt SrcPos)
checkBlock (BlockStmt stmts) innerTable = do
    fName <- use currentFun
    fTable <- use funcDefs
    scopeVars <- M.union <$> use curVars <*> use prevVars

    -- Create and type check new inner block, and add to current
    -- list of inner blocks if successful
    let newBlock = CodeBlock fName fTable scopeVars innerTable
    lift $ runBlockCheck stmts newBlock


-- | Type check return stmt
checkReturn :: Expr SrcPos -> BlockState (Stmt SrcPos)
checkReturn expr = do
    fName <- use currentFun
    fTable <- use funcDefs
    scopeVars <- M.union <$> use curVars <*> use prevVars

    let notFound = "Error, function not found " ++ show fName

    (retType, _) <- lift $ note notFound $ M.lookup fName fTable
    exprType <- lift $ getTypeExpr scopeVars fTable expr

    checkType retType exprType
    return $ Return expr


checkMethodCall :: MethodCall SrcPos -> BlockState (MethodCall SrcPos)
checkMethodCall (MethodCall var method args) = do
    vTable <- M.union <$> use curVars <*> use prevVars
    fTable <- use funcDefs
    scopeVars <- M.union <$> use curVars <*> use prevVars
    
    case var of
        (VarIdent i) -> do
            gType <- lift $ findType i vTable
            checkType gType objectType
            return $ MethodCall var method args           
            
        -- Check we're accessing an element of an array of objects,
        -- and that the element is an integer type
        (VarArrayElem i el) -> do
            gType <- lift $ findType i vTable
            elemType <- lift $ getTypeExpr scopeVars fTable el
            checkType elemType (intType False)
            checkType  gType arrayObjType
            return $ MethodCall var method args


  where findType i vTable = note (notFound i) (M.lookup i vTable)
        notFound  obj = "Error, object not found " ++ show obj


-- | Checks that 2 given types match
checkType :: (Show a) =>  Type SrcPos -> Type a -> GenericBlockState b ()
checkType expected actual =
    if stripAnnType expected == stripAnnType actual then modify id else
        lift $ Left $ errorType expected ++ show expected ++ 
            " but expression evaluated to " ++ show actual


checkType' :: (Show a) => Type SrcPos -> Type a -> Either String ()
checkType' expected actual =
    if stripAnnType expected == stripAnnType actual then Right () else
        Left $ errorType expected ++ show expected ++ " but expression evaluated to " ++ show actual


-- | Given an identity and var table, if the identity is already
-- part ofr the variable table, then returns a multiple instance error
checkMultipleInstance :: Ident SrcPos -> VarTable -> GenericBlockState a ()
checkMultipleInstance ident vTable = when (ident `M.member` vTable) $ 
    lift $ Left $ errorIdent ident ++ show ident ++ 
    " has already been defined in scope, cannot redefine it"


-- | Obtain Type of Expression, returns error message
-- | if types arn't consistent, or identifiers arn't in scope
getTypeExpr :: VarTable -> FunTable -> Expr SrcPos -> Either String (Type SrcPos)
getTypeExpr vtable ftable expr = case expr of
    (ExpBinOp b e1 e2) -> getTypeBinOp vtable ftable b e1 e2
    (ExpUnaryOp u e) -> getTypeUnOp vtable ftable u e
    (ExpFunCall (FunCall s exps)) -> do
        argTypes <- mapM (getTypeExpr vtable ftable) exps
        (retT, ts) <- note (notFound s) (M.lookup s ftable)
        if length argTypes /= length ts
            then Left $ errorIdent s ++ "Function " ++ show s ++  " expects " ++ show (length ts) ++
                      " arguments but was given " ++ show (length argTypes)
            else if argTypes /= ts
                then Left $ errorIdent s ++ "Arguments don't evaluate to given types"
                else Right retT

    (ExpMethodCall (MethodCall var _ args)) -> do
        _ <- mapM (getTypeExpr vtable ftable) args

        case var of
            (VarIdent i@(Ident sp s)) -> do
                objType <- note (notFound i) (M.lookup i vtable)
                checkType' objType objectType
                return $ NormalType sp inKernel "object"
            
            (VarArrayElem i@(Ident sp s) el) -> do
                objType <- note (notFound i) (M.lookup i vtable)
                elemType <- getTypeExpr vtable ftable el
                checkType' elemType (intType False)
                checkType' objType arrayObjType
                return $ NormalType sp inKernel "object"

    (ExpIdent i) -> note (notFound i) (M.lookup i vtable)

    (ExpPointer (Pointer ident@(Ident sp i) n)) ->
        PointerType <$> note (notFound ident) (M.lookup catIdent vtable)
          where catIdent = Ident sp (show n ++ i)

    (ExpLit l) -> return $ case l of
                Str s _ -> strTypePos notKernel s
                Ch s _ -> chTypePos notKernel s
                Number s (Left _) -> intTypePos notKernel s
                Number s (Right _) -> doubleTypePos notKernel s
                Bl s _ -> boolTypePos notKernel s

 where
    notFound :: Ident SrcPos -> String 
    notFound i = errorIdent i ++ "Identifier " ++ show i ++ " not declared in scope"


-- Get Type of a Binary Expression
getTypeBinOp :: VarTable -> FunTable -> BinOps SrcPos -> Expr SrcPos 
                         -> Expr SrcPos -> Either String (Type SrcPos)
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
getNormalTypeBin :: BinOps SrcPos -> Type SrcPos -> Type SrcPos -> Either String (Type SrcPos)
getNormalTypeBin bop leftType rightType
   | bop `elem` numNumNumOp =
       if leftType /= rightType then
           Left $ errorBinOp bop ++ "Both expressions expected to be the same type"
       else if stripAnnType leftType == intType' then return $ intTypePos kernel opPos
       else if stripAnnType leftType == doubleType' then return $ doubleTypePos kernel opPos
       else Left "Expected integer or double type"

   | bop `elem` intIntIntOp =
       if (stripAnnType leftType, stripAnnType rightType) == (intType', intType')
           then return $ intTypePos kernel opPos
           else Left "Expected integer values on both sides"

   | bop `elem` compareOp =
       if (stripAnnType leftType, stripAnnType rightType) `elem` 
            [(intType', intType'), (doubleType', doubleType')]
           then return $ boolTypePos kernel opPos
           else Left "Expected numeric values of the same type"

   | bop `elem` boolOp =
       if (stripAnnType leftType, stripAnnType rightType) == (boolType', boolType')
           then return $ boolTypePos kernel opPos
           else Left "Expected boolean values"

   | bop `elem` eqOp = 
        if leftType == rightType 
            then return $ boolTypePos kernel opPos
            else Left "Expected equality of same types"

  where
     numNumNumOp = [Add opPos, Sub opPos, Mul opPos, Div opPos]
     intIntIntOp = [Mod opPos, BAnd opPos, BOr opPos, BXor opPos, ShiftL opPos, ShiftR opPos]
     compareOp = [LessEq opPos, Less opPos, Greater opPos, GreaterEq opPos]
     eqOp = [Equals opPos, NEquals opPos]
     boolOp = [And opPos, Or opPos]
     intType' = intType kernel
     boolType' = boolType kernel
     doubleType' = doubleType kernel
     kernel = isInKernel leftType
     opPos = binOpPos bop


-- Get type of binary expression involving pointers
getPointerTypeBin :: BinOps SrcPos -> Type SrcPos -> Type SrcPos -> Either String (Type SrcPos)
getPointerTypeBin bop leftType rightType
    | bop == Add opPos =
        if isPointer leftType && (stripAnnType rightType) == intType' then
            return leftType
        else if isPointer rightType && (stripAnnType leftType) == intType' then
            return rightType
        else Left "Can only add Pointers to Integers"

    | bop == Sub opPos =
        if isPointer leftType && (stripAnnType rightType) == intType' then
        return leftType
        else Left "expected pointer type on lhs and integer type on rhs for pointer subtraction"
    | bop `elem` [Equals opPos, NEquals opPos] = case (leftType, rightType) of
        ((PointerType a, PointerType b)) ->
            if a == b then
                return $ boolTypePos kernel opPos
            else Left $ "Expected pointer types to be equal, left points to " ++ show a ++
                  ". Right points to " ++ show b ++ "."
        _ -> Left "Cannot perform an equality comparison of pointer and non pointer types"

    | otherwise =  Left $ "operation " ++ show bop ++ " not defined for pointer types"
  where
     intType' = intType kernel
     boolType' = boolType kernel
     kernel = isInKernel leftType
     opPos = binOpPos bop


--Get type of unary expression
getTypeUnOp :: VarTable -> FunTable -> UnaryOps SrcPos -> Expr SrcPos -> Either String (Type SrcPos)
getTypeUnOp vtable ftable operation expr = case operation of

    BNot p -> getTypeExpr vtable ftable expr >>=
        \t -> case t of
            (NormalType _ kernel "int") -> return $ NormalType p kernel "int"
            e -> Left $ errorType t ++ "Expected integer expression, but found" ++ show e

    Neg p -> getTypeExpr vtable ftable expr >>=
        \t -> case t of
            (NormalType _ kernel "int") -> return $ NormalType p kernel "int"
            (NormalType _ kernel "double") -> return $ NormalType p kernel "double"
            e -> Left $ errorType t ++ "Expected integer expression, but found" ++ show e

    Not p -> getTypeExpr vtable ftable expr >>=
        \t -> case t of
            (NormalType _ kernel "bool") -> return $ NormalType p kernel "bool"
            e -> Left $ errorType t ++ "Expected boolean expression, but found " ++ show e



errorSrcPos :: SrcPos -> String
errorSrcPos (SrcPos line col) = "Error " ++ show line ++ ":" ++ show col ++ "\n"

errorIdent :: Ident SrcPos -> String
errorIdent (Ident sp _) = errorSrcPos sp


errorType :: Type SrcPos -> String
errorType (PointerType t) = errorType t
errorType (NormalType sp _ _) = errorSrcPos sp

stripAnnType :: Type a -> Type ()
stripAnnType (PointerType t) = PointerType $ stripAnnType t
stripAnnType (NormalType _ a b) = NormalType () a b

binOpPos :: BinOps SrcPos -> SrcPos
binOpPos bop = case bop of
      Add a -> a
      Sub a -> a
      Mul a -> a
      Div a -> a
      And a -> a
      Or a  -> a
      Mod a -> a
      Less a  -> a
      LessEq a -> a
      Greater a -> a
      GreaterEq a -> a
      Equals a  -> a
      NEquals a -> a
      ShiftL a -> a
      ShiftR a -> a
      BAnd a -> a
      BXor a -> a
      BOr a -> a

errorBinOp :: BinOps SrcPos -> String
errorBinOp = errorSrcPos . binOpPos 

errorUnOp :: UnaryOps SrcPos -> String
errorUnOp = errorSrcPos . unOpPos

unOpPos :: UnaryOps SrcPos -> SrcPos
unOpPos op = case op of
    Not a -> a
    Neg a -> a
    BNot a -> a 


