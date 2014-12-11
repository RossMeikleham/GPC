{-# LANGUAGE TemplateHaskell #-}
{- Generate GPIR code from Type/Scope checked AST -}


module GPC.GenGPIR (genGPIR) where

import Control.Lens
import Control.Applicative hiding ((<|>), many, optional, empty)
import Control.Monad.State.Lazy
import Control.Error.Util
import qualified Data.Map as M
import GPC.AST
import GPC.GPIRAST

type ConstVarTable = M.Map Ident Literal
type FunTable = M.Map Ident ([Ident], BlockStmt)
type VarRegTable = M.Map Ident Integer

data CodeGen = CodeGen {
   _funTable :: FunTable,  -- ^ Store symbol tree for functions
   _constTable :: ConstVarTable,
   _varId :: Integer, -- ^ Current variable id for mapping to registers 
   _varRegTable :: VarRegTable -- ^ maps variable identifier
}

-- Create lenses to access Block fields easier
makeLenses ''CodeGen

type GenState a = StateT CodeGen (Either String) a 

isAssign :: TopLevel -> Bool
isAssign tl = case tl of 
    TLAssign _ -> True
    _ -> False

isObject :: TopLevel -> Bool
isObject tl = case tl of
    TLObjs _ -> True
    _ -> False

isNonMainFunc :: TopLevel -> Bool
isNonMainFunc tl = case tl of
    Func _ name _ _ -> name /= Ident "main" 
    _ -> False

-- | Update the register table and counter
-- | return the next available register
updateRegs :: Ident -> GenState Ident
updateRegs ident = do  
    vId <- use varId
    vRegTable <- use varRegTable
    let newVarId = vId + 1
    assign varId newVarId
    assign varRegTable $ M.insert ident newVarId vRegTable
    return $ Ident (show newVarId)

genGPIR :: Program -> Either String SymbolTree
genGPIR (Program tls) = case runStateT (genTopLevel tls) initial of 
    Left s -> Left s
    (Right (tl, _)) -> Right $ tl
 where initial = CodeGen M.empty M.empty 0 M.empty


genTopLevel :: [TopLevel] -> GenState SymbolTree
genTopLevel tls = do
    genTLAssigns tls 
    genFuncs tls
    let tls' = filter (\x -> not (isAssign x || isObject x || isNonMainFunc x)) tls
    symbolTrees <- mapM genTopLevelStmt tls'
    if length symbolTrees > 1 
        then return $ SymbolList False (seqSymbol : symbolTrees)
        else return $ SymbolList False symbolTrees
 where 
    seqSymbol = Symbol $ GOpSymbol $
                MkOpSymbol False ("Dummy", 0) "CoreServices" "Seq" "seq"

-- | Generate all Top Level Assignments
genTLAssigns :: [TopLevel] -> GenState ()
genTLAssigns tls = mapM_ genTLAssign $ filter isAssign tls


genTLAssign :: TopLevel -> GenState ()
genTLAssign (TLAssign (Assign _ ident expr)) = case expr of
    (ExpLit l) -> do         
        cTable <- use constTable
        assign constTable $ M.insert ident l cTable 
    (ExpPointer _) -> return ()
    _ -> lift $ Left $ (show expr) --"Compiler error, in top level assignment code generation"  
genTLAssign _ = lift $ Left $ "Not top level Assignment statement"


genFuncs :: [TopLevel] -> GenState ()
genFuncs tls = mapM_ genFunc $ filter isNonMainFunc tls  

genFunc :: TopLevel -> GenState ()
genFunc (Func _ name args stmts) = do
    fTable <- use funTable
    assign funTable $ M.insert name (map snd args, stmts) fTable 
         
genFunc _ = lift $ Left $ "Not Function definition"


-- | Generate GPIR from Top Level statements
genTopLevelStmt :: TopLevel -> GenState SymbolTree
genTopLevelStmt tl = case tl of
      (TLConstructObjs cObjs) -> genTLConstructObjs cObjs
      (Func _ (Ident "main")  _ bStmt) -> genMain bStmt
      _ -> lift $ Left $ "Compiler error, shouldn't contain Top Level " ++ 
                "Assignments, Object Decls or non Main functions"


         
-- | Generate Object Constructors
genTLConstructObjs :: ConstructObjs -> GenState SymbolTree
genTLConstructObjs (ConstructObjs var libName cName exprs) =  
    case var of  
        (VarIdent _) -> do
            let constructor = Symbol $ GOpSymbol $ 
                            MkOpSymbol False ("dummy", 0) (show libName) (show cName) (show cName)
            args <- mapM checkConst exprs
            let args' = map (\x -> Symbol (ConstSymbol True (show x))) args
            return $ SymbolList True (constructor : args')

        -- TODO work out how to map
        (VarArrayElem _ indexExpr) -> do
            _ <- checkConst indexExpr
            let constructor = Symbol $ GOpSymbol $ 
                           MkOpSymbol False ("dummy", 0) (show libName) (show cName) (show cName)
            args <- mapM checkConst exprs
            let args' = map (\x -> Symbol (ConstSymbol True (show x))) args
            return $ SymbolList True (constructor : args')



-- | Generate Program
genMain :: BlockStmt -> GenState SymbolTree
genMain (BlockStmt stmts) = (SymbolList True) <$> (mapM genStmt stmts)

genStmt :: Stmt -> GenState SymbolTree
genStmt stmt = case stmt of

    -- When assigning a variable need to write it to a register
    AssignStmt (Assign _ name expr) -> do
        let assignSymbol = Symbol $ GOpSymbol $ 
                            MkOpSymbol False ("", 0) "CoreServices" "reg" "write"
        expr' <- genExpr expr
        reg <- updateRegs name
        let regSymbol = Symbol $ ConstSymbol True (show reg)
        return $ SymbolList False [assignSymbol, regSymbol,  expr']

    Seq (BlockStmt stmts) -> do
        let seqSymbol = Symbol $ GOpSymbol $
                        MkOpSymbol False ("Dummy", 0) "CoreServices" "Seq" "seq"
        stmts' <- mapM genStmt stmts
        return $ SymbolList False (seqSymbol : stmts')

    BStmt (BlockStmt stmts) -> do
        stmts' <- mapM genStmt stmts
        return $ SymbolList False stmts'

    FunCallStmt (FunCall name exprs) -> do
        (BlockStmt stmts) <- genInlineFunc name exprs
        stmts' <- mapM genStmt stmts
        return $ SymbolList False stmts'

    MethodStmt (MethodCall cName mName exprs) -> do
        let call = Symbol $ GOpSymbol $ 
                    MkOpSymbol False ("Dummy", 0) "temp" (show cName) (show mName)
        exprs' <- mapM genExpr exprs
        return $ SymbolList False (call : exprs')

    If expr stmt' -> do
        let ifSymbol = Symbol $ GOpSymbol $
                        MkOpSymbol False ("Dummy", 0) "CoreServices" "IF" "if"
        cond <- genExpr expr
        stmt'' <- genStmt stmt'
        let dummyStmt = Symbol $ ConstSymbol True "0"
        return $ SymbolList False [ifSymbol, cond, stmt'', dummyStmt]
        
    IfElse expr stmt1 stmt2 -> do                
        let ifSymbol = Symbol $ GOpSymbol $
                        MkOpSymbol False ("Dummy", 0) "CoreServices" "IF" "if"
        cond <- genExpr expr
        stmt1' <- genStmt stmt1
        stmt2' <- genStmt stmt2
        return $ SymbolList False [ifSymbol, cond, stmt1', stmt2']
       
    Return expr -> do
        let returnSymbol = Symbol $ GOpSymbol $
                            MkOpSymbol False ("Dummy", 0) "CoreServices" "RETURN" "return"
        expr' <- genExpr expr
        return $ SymbolList False [returnSymbol, expr']
        
    -- For now fully loop unroll 
    ForLoop ident start stop step stmt' -> do
       start' <- getInt start
       stop'  <- getInt stop
       step'  <- getInt step
       if (start' > stop') then lift $ Left $ "For loop error, start can't be greater than stop"
       else if (step' == 0 || step' < 0) then lift $ Left $ "For loop error, infinite loop generated"
       else do
           let unrolledStmts = map (\i -> 
                                genInlineStmt [(ident, ExpLit (Number (Left i)))] (BStmt stmt'))  
                                    [start', start' + step' .. stop' - 1]
           unrolledStmts' <- mapM genStmt unrolledStmts
           return $ SymbolList False unrolledStmts'

         
 where 
    getInt :: Expr -> GenState Integer
    getInt (ExpLit (Number (Left i))) = return i
    getInt _ = lift $ Left $ "Compiler error, expected integer value from expression"

genExpr :: Expr -> GenState SymbolTree
genExpr expr = case expr of
    
    ExpBinOp bOp lExpr rExpr -> do
        let method = case bOp of
                    Add -> "plus"
                    Sub -> "minus"
                    Mul -> "times"
                    Div -> "over"
                    And -> "and"
                    Or -> "or"
                    Mod -> "mod"
                    Less -> "lt"
                    LessEq -> "lteq"
                    Greater -> "gt"
                    GreaterEq -> "gteq"
                    Equals -> "eq"
                    NEquals -> "neq"
                    ShiftL -> "shl"
                    ShiftR -> "shr"
                    BAnd -> "band"
                    BXor -> "bxor"
                    BOr -> "bor"

        let binSymbol = Symbol $ GOpSymbol $
                        MkOpSymbol False ("Dummy", 0) "CoreServices" "ALU" method 

        lExpr' <- genExpr lExpr
        rExpr' <- genExpr rExpr
        return $ SymbolList False [binSymbol, lExpr', rExpr']

    ExpUnaryOp unOp expr' -> do
        let method = case unOp of
                    Not -> "not"
                    Neg -> "neg"
                    BNot -> "bnot"
                    _ -> error "undefined operation"

        let unSymbol = Symbol $ GOpSymbol $
                    MkOpSymbol False ("Dummy", 0) "CoreSevices" "ALU" method
        expr'' <- genExpr expr'
        return $ SymbolList False [unSymbol, expr'']

    ExpFunCall (FunCall name exprs) -> do
        (BlockStmt stmts) <- genInlineFunc name exprs
        stmts' <- mapM genStmt stmts
        return $ SymbolList False stmts'

    ExpMethodCall (MethodCall cName mName exprs) -> do
        let call = Symbol $ GOpSymbol $ 
                    MkOpSymbol False ("Dummy", 0) "temp" (show cName) (show mName)
        exprs' <- mapM genExpr exprs
        return $ SymbolList False (call : exprs')

    -- Encounterd a variable, need to read it from its register
    ExpIdent ident -> do 
        regTable <- use varRegTable
        reg <- lift $ note regNotFound $ (M.lookup ident regTable) 
        let regSymbol = Symbol $ GOpSymbol $ 
                        MkOpSymbol False ("", 0) "CoreServices" "Reg" "read"
        return $ SymbolList False  [regSymbol, Symbol $ ConstSymbol True (show reg)]
     where regNotFound = "Compiler error, register for ident " ++ (show ident) ++ "not found"

    ExpLit lit -> return $ Symbol $ ConstSymbol True (show lit)
    
    ExpPointer p -> return EmptyTree

-- | Generate Inline Function by replacing all identifieres
-- | in scope with supplied argument expressions
genInlineFunc :: Ident -> [Expr] -> GenState BlockStmt
genInlineFunc name args = do
    fTable <- use funTable 
    case M.lookup name fTable of 
        Just (idents, stmts) -> do
            let (BStmt stmts') = genInlineStmt (zip idents args) (BStmt stmts)
            return stmts'
            
        Nothing -> lift $ Left $ "Compiler error genInlineFunc"


genInlineStmt :: [(Ident, Expr)] -> Stmt -> Stmt
genInlineStmt exprs stmt = case stmt of

    AssignStmt (Assign gType name expr) -> 
        AssignStmt (Assign gType name $ replaceExprIdents exprs expr)

    FunCallStmt (FunCall name args) ->
        FunCallStmt (FunCall name $ map (replaceExprIdents exprs) args)

    MethodStmt (MethodCall cName mName args) ->
        MethodStmt (MethodCall cName mName $ map (replaceExprIdents exprs) args)

    If expr stmt' ->
        If (replaceExprIdents exprs expr) (genInlineStmt exprs stmt')
        
    IfElse expr stmt1 stmt2 -> 
        IfElse (replaceExprIdents exprs expr) (genInlineStmt exprs stmt1) 
                                              (genInlineStmt exprs stmt2)

    Return expr -> Return $ replaceExprIdents exprs expr

    ForLoop name start stop step (BlockStmt stmts) -> 
        ForLoop name (replaceExprIdents exprs start)
                     (replaceExprIdents exprs stop)
                     (replaceExprIdents exprs step)
                     (BlockStmt $ genBlock stmts) 


    Seq (BlockStmt stmts) -> Seq $ BlockStmt $ genBlock stmts

    BStmt (BlockStmt stmts) -> BStmt $ BlockStmt $ genBlock stmts

  where 
    -- Once the identifier goes out of scope we don't replace it
    -- map up until the identifier is out of scope, and then add on the rest of
    -- the unmapped statements to the block as they won't need any substitution 
    genBlock :: [Stmt] -> [Stmt]
    genBlock stmts = mappedVals ++ (drop (length mappedVals) stmts)
      where mappedVals = incMapWhile inScope (genInlineStmt exprs) stmts
    inScope (AssignStmt (Assign _ name _)) = 
        if name `elem` idents then False else True
    inScope _ = True 
    idents = map fst exprs


replaceExprIdents :: [(Ident, Expr)] -> Expr -> Expr
replaceExprIdents replaceExprs givenExpr = 
    foldl (\gExpr (ident, rExpr) -> replaceExprIdent ident rExpr gExpr) givenExpr replaceExprs

-- | Replace all instances of an identity in an expression
-- | with a given sub-expression TODO place into expression module
replaceExprIdent :: Ident -> Expr -> Expr -> Expr
replaceExprIdent ident replaceExpr givenExpr = case givenExpr of

    ExpBinOp binOp lExpr rExpr -> 
        ExpBinOp binOp (replaceExprIdent ident replaceExpr lExpr)
                       (replaceExprIdent ident replaceExpr rExpr)

    ExpUnaryOp unOp expr -> ExpUnaryOp unOp (replaceExprIdent ident replaceExpr expr)

    ExpFunCall (FunCall name exprs) ->
        ExpFunCall (FunCall name $ 
            map (replaceExprIdent ident replaceExpr) exprs)

    ExpMethodCall (MethodCall cName mName exprs) ->
        ExpMethodCall (MethodCall cName mName $ 
            map (replaceExprIdent ident replaceExpr) exprs)

    ExpIdent expId -> if expId == ident then replaceExpr else (ExpIdent expId)

    ExpLit lit -> ExpLit lit
    
    ExpPointer p -> ExpPointer p

-- | inclusive MapWhile function, returns results which satisfy a condition
-- | TODO stick in utilities module
incMapWhile :: (b -> Bool) -> (a -> b) -> [a] -> [b]
incMapWhile cond f (x:xs) = if cond res then res : (incMapWhile cond f xs) else [res]
 where res = f x
incMapWhile _ _ [] = []


checkConst :: Expr -> GenState Literal
checkConst expr = case expr of 
    (ExpLit l) -> return l
    _ -> lift $ Left $ "Expected constant expression"
