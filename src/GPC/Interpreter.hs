{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{- Generate GPIR code from Type/Scope checked AST -}


module GPC.Interpreter(genGPIR) where 


import           Control.Applicative      hiding (empty, many, optional, (<|>))
import           Control.Error.Util
import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.Bits
import qualified Data.Map                 as M
import           GPC.TypelessAST
import           GPC.GPIRAST

type ConstVarTable = M.Map Ident Literal
type FunTable = M.Map Ident ([Ident], BlockStmt)
type VarRegTable = M.Map Ident Integer

data CodeGen = CodeGen {
   _funTable    :: FunTable,  -- ^ Store symbol tree for functions
   _constTable  :: ConstVarTable,
   _varId       :: Integer, -- ^ Current variable id for mapping to registers
   _varRegTable :: VarRegTable, -- ^ maps variable identifier
   _threadCount :: Integer, -- ^ Current thread number to map
   _maxThreads  :: Integer  -- ^ Maximum number of threads
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

isFunc :: TopLevel -> Bool
isFunc tl = case tl of
    Func{} -> True
    _ -> False

isNonEntryFunc :: String -> TopLevel -> Bool
isNonEntryFunc eName tl = case tl of
    Func fName _ _ -> fName /= Ident eName
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

-- | Obtain the current thread
--   and increment the thread counter modulo the max no of threads
getThread :: GenState Integer
getThread = do
    threadNo <- use threadCount
    maxT <- use maxThreads
    assign threadCount $ (threadNo + 1) `mod` maxT
    return threadNo 

genGPIR :: String -> Program -> Integer -> Either String SymbolTree
genGPIR name (Program tls) threads = case runStateT (genTopLevel name tls) initial of
    Left s -> Left s
    (Right (tl, _)) -> Right tl
 where initial = CodeGen M.empty M.empty 0 M.empty 0 threads


genTopLevel :: String -> [TopLevel] -> GenState SymbolTree
genTopLevel name tls = do
    genTLAssigns tls
    genFuncs tls
    let tls' = filter (\x -> not (isAssign x || isObject x || isNonEntryFunc name x)) tls
    symbolTrees <- mapM genTopLevelStmt tls'
    return $ SymbolList False $ seqSymbol : symbolTrees
 where
    seqSymbol = Symbol $ ConstSymbol False "begin"


-- | Generate all Top Level Assignments
genTLAssigns :: [TopLevel] -> GenState ()
genTLAssigns tls = mapM_ genTLAssign $ filter isAssign tls

genTLAssign :: TopLevel -> GenState ()
genTLAssign (TLAssign (Assign ident expr)) = case expr of
    (ExpLit l) -> do
        cTable <- use constTable
        assign constTable $ M.insert ident l cTable
    _ -> lift $ Left $ show expr --"Compiler error, in top level assignment code generation"
genTLAssign _ = lift $ Left "Not top level Assignment statement"


-- | Store all functions, to be evaluated later
genFuncs :: [TopLevel] -> GenState ()
genFuncs tls = mapM_ genFunc $ filter isFunc tls

genFunc :: TopLevel -> GenState ()
genFunc (Func name args stmts) = do
    fTable <- use funTable
    assign funTable $ M.insert name (args, stmts) fTable

genFunc a = lift $ Left $ "Not Function definition" ++ show a


-- | Generate GPIR from Top Level statements
genTopLevelStmt :: TopLevel -> GenState SymbolTree
genTopLevelStmt tl = case tl of
      (TLConstructObjs cObjs) -> genTLConstructObjs cObjs
      (Func _ args bStmt) -> genNest $ genEntryFunc bStmt args
      _ -> lift $ Left $ "Compiler error, shouldn't contain Top Level " ++
                "Assignments, Object Decls or non Entry functions"


-- | Generate Object Constructors
genTLConstructObjs :: ConstructObjs -> GenState SymbolTree
genTLConstructObjs (ConstructObjs ns var exprs) = do
    let constructor = Symbol $ GOpSymbol $
                    MkOpSymbol False ("", 0) (map show ns)
    args <- mapM checkConst exprs
    let args' = map (Symbol . ConstSymbol True . show) args

    case var of
        -- TODO work out how to map
        (VarArrayElem _ indexExpr) -> void $ checkConst indexExpr
        (VarIdent _) -> return ()

    return $ SymbolList True (constructor : args')


-- | Generate Program
genEntryFunc :: BlockStmt -> [Ident] ->  GenState SymbolTree
genEntryFunc (BlockStmt stmts) args = do
    mapM_ updateRegs args -- Create placeholder for entry args 
    SymbolList True <$> mapM genStmt (takeWhileInclusive (not . isReturn) stmts)


-- | Function to deal with state and nesting
--  we want to give a new context which constant variables are overwritten
--  in the new scope when another is declared, or are erased when a non constant
--  variable of the same name is declared. We also want to keep a running count of
--  the registers used
genNest :: GenState a -> GenState a
genNest g = do
    curEnv <- get
    case runStateT g curEnv of
        Left e -> lift $ Left e
        Right (res, afterEnv) -> do
            assign varId (_varId afterEnv)
            return res



genStmt :: Stmt -> GenState SymbolTree
genStmt s = case s of

    -- When assigning a variable need to write it to a register
    AssignStmt (Assign name expr) -> do
        expr' <- reduceExpr expr
        case expr' of
            -- Values that evaluate to literals will not need to be written
            -- to registers, wherever they are used can be directly
            -- substituted during compliation
            ExpLit l -> do 
                constTable %= M.insert name l
                return EmptyTree
            -- Otherwise store the expression in a register
            -- to be read from when used elsewhere
            _ -> do
                -- If variable non constant, if there exists an entry for a variable in the
                --  above scope of the same name in the constant table, erase it
                constTable %= M.delete name

                let assignSymbol = Symbol $ GOpSymbol $
                            MkOpSymbol False ("", 0) ["CoreServices", "Reg", "write"]
                evalExpr <- genExpr expr
                reg <- updateRegs name
                let regSymbol = Symbol $ ConstSymbol True (filter (/='"') (show reg))
                return $ SymbolList False [assignSymbol, regSymbol,  evalExpr]

    Seq (BlockStmt stmts) -> do
        let seqSymbol = Symbol $ GOpSymbol $
                        MkOpSymbol False ("Dummy", 0) ["seq"]
        -- Entering a block, so need to nest 
        stmts' <- genNest $ mapM genStmt (takeWhileInclusive (not . isReturn) stmts)
        return $ SymbolList False (seqSymbol : stmts')

    BStmt (BlockStmt stmts) -> do
        -- Entering a block, need to nest
        stmts' <- genNest $ mapM genStmt (takeWhileInclusive (not . isReturn) stmts)
        return $ SymbolList False stmts'

    FunCallStmt (FunCall name exprs) -> do
        exprs' <- mapM reduceExpr exprs
        (BlockStmt stmts) <- genInlineFunc name exprs'
        stmts' <- genNest $ mapM genStmt (takeWhileInclusive (not . isReturn) stmts)
        return $ SymbolList False stmts'

    MethodStmt (MethodCall cName method exprs) -> do
        exprs' <- mapM reduceExpr exprs
        let call = Symbol $ GOpSymbol $
                    MkOpSymbol False ("Dummy", 0) [show cName, filter (/='"') $ show method]
        evalExprs <- mapM genExpr exprs'
        return $ SymbolList False (call : evalExprs)

    If expr stmt -> do
        expr' <- reduceExpr expr
        case expr' of
            -- If Expression is a literal boolean we can evaluate the
            -- branch at compile time
            (ExpLit (Bl b)) -> 
                if b 
                  then genStmt stmt                   
                  else return EmptyTree

            -- Otherwise generate code to evaluate at runtime
            _ -> do
                let ifSymbol = Symbol $ GOpSymbol $
                        MkOpSymbol False ("Dummy", 0) ["if"]

                cond <- genExpr expr'
                evalStmt <- genStmt stmt
                -- GPRM supports only if-then-else style, 
                --  generate dummy statement to fill in the "else"
                let dummyStmt = Symbol $ ConstSymbol True "0"
                return $ SymbolList False [ifSymbol, cond, evalStmt, dummyStmt]

    IfElse expr stmt1 stmt2 -> do
        expr' <- reduceExpr expr
        case expr' of
            --Again like the If expression see if we can evaluate
            -- which branch to take during compile time
            (ExpLit (Bl b)) -> 
                if b 
                  then genStmt stmt1
                  else genStmt stmt2

            _ -> do
                let ifSymbol = Symbol $ GOpSymbol $
                        MkOpSymbol False ("Dummy", 0) ["if"]
                cond <- genExpr expr'
                evalStmt1 <- genStmt stmt1
                evalStmt2 <- genStmt stmt2
                return $ SymbolList False [ifSymbol, cond, evalStmt1, evalStmt2]

    Return expr -> do
        expr' <- reduceExpr expr
        evalExpr <- genExpr expr'
        return $ SymbolList False [evalExpr]

    -- For now fully loop unroll
    ForLoop ident start stop step stmt' -> do
       start' <- getInt =<< reduceExpr start
       stop'  <- getInt =<< reduceExpr stop 
       step'  <- getInt =<< reduceExpr step
       if start' > stop' then lift $ Left "For loop error, start can't be greater than stop"
       else if step' == 0 || step' < 0 then lift $ Left "For loop error, infinite loop generated"
       else do
           let unrolledStmts = map (\i ->
                                genInlineStmt [(ident, ExpLit (Number (Left i)))] (BStmt stmt'))
                                    [start', start' + step' .. stop' - 1]
           unrolledStmts' <- mapM genStmt (takeWhileInclusive (not . isReturn) unrolledStmts)
           return $ SymbolList False unrolledStmts'


 where
    getInt :: Expr -> GenState Integer
    getInt (ExpLit (Number (Left i))) = return i
    getInt _ = lift $ Left "Compiler error, expected integer value from expression"


genExpr :: Expr -> GenState SymbolTree
genExpr e = do 
    expr <- reduceExpr e
    case expr of

     ExpBinOp bOp lExpr rExpr -> do
        let method = case bOp of
                    Add -> "+"
                    Sub -> "-"
                    Mul -> "*"
                    Div -> "/"
                    And -> "&&"
                    Or -> "||"
                    Mod -> "%"
                    Less -> "<"
                    LessEq -> "<="
                    Greater -> ">"
                    GreaterEq -> ">="
                    Equals -> "=="
                    NEquals -> "!="
                    ShiftL -> "<<"
                    ShiftR -> ">>"
                    BAnd -> "&"
                    BXor -> "^"
                    BOr -> "|"

        let binSymbol = Symbol $ GOpSymbol $
                        MkOpSymbol False ("Dummy", 0) [method]

        lExpr' <- genExpr lExpr
        rExpr' <- genExpr rExpr
        return $ SymbolList False [binSymbol, lExpr', rExpr']

     ExpUnaryOp unOp expr' -> do
        let method = case unOp of
                    Not -> "!"
                    Neg -> "-"
                    BNot -> "~"

        let unSymbol = Symbol $ GOpSymbol $
                    MkOpSymbol False ("Dummy", 0) [method]
        expr'' <- genExpr expr'
        return $ SymbolList False [unSymbol, expr'']

     ExpFunCall (FunCall name exprs) -> do
        (BlockStmt stmts) <- genInlineFunc name exprs
        stmts' <- genNest $ mapM genStmt (takeWhileInclusive (not . isReturn) stmts)
        return $ SymbolList False stmts'

     ExpMethodCall (MethodCall cName method exprs) -> do
        let call = Symbol $ GOpSymbol $
                    MkOpSymbol False ("Dummy", 0) [show cName, filter (/='"') $ show method]
        exprs' <- mapM genExpr exprs
        return $ SymbolList False (call : exprs')

     -- Encountered a variable, need to read it from its register
     ExpIdent ident -> do
        regTable <- use varRegTable
        reg <- lift $ note regNotFound $ M.lookup ident regTable
        let regSymbol = Symbol $ GOpSymbol $
                        MkOpSymbol False ("", 0) ["CoreServices", "Reg", "read"]
        return $ SymbolList False  [regSymbol, Symbol $ ConstSymbol True (show reg)]
      where regNotFound = "Compiler error, register for ident " ++ show ident ++ "not found"

     ExpLit lit -> return $ Symbol $ ConstSymbol True (show lit)


-- | Generate Inline Function by replacing all identifiers
-- | in scope with supplied argument expressions
genInlineFunc :: Ident -> [Expr] -> GenState BlockStmt
genInlineFunc name args = do
    fTable <- use funTable
    case M.lookup name fTable of
        Just (idents, stmts) -> do
            let (BStmt stmts') = genInlineStmt (zip idents args) (BStmt stmts)
            return stmts'

        Nothing -> lift $ Left "Compiler error genInlineFunc"


genInlineStmt :: [(Ident, Expr)] -> Stmt -> Stmt
genInlineStmt exprs stmt = case stmt of

    AssignStmt (Assign name expr) ->
        AssignStmt (Assign name $ replaceExprIdents exprs expr)

    FunCallStmt (FunCall name args) ->
        FunCallStmt (FunCall name $ map (replaceExprIdents exprs) args)

    MethodStmt (MethodCall cName method args) ->
        MethodStmt (MethodCall cName method $ map (replaceExprIdents exprs) args)

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
    genBlock stmts = mappedVals ++ drop (length mappedVals) stmts
      where mappedVals = incMapWhile inScope (genInlineStmt exprs) stmts
    inScope (AssignStmt (Assign name _)) = name `notElem` idents
    inScope _ = True
    idents = map fst exprs


replaceExprIdents :: [(Ident, Expr)] -> Expr -> Expr
replaceExprIdents replaceExprs givenExpr =
    foldl (\gExpr (ident, rExpr) -> replaceExprIdent ident rExpr gExpr) givenExpr replaceExprs

-- | Replace all instances of an identity in an expression
-- | with a given sub-expression 
replaceExprIdent :: Ident -> Expr -> Expr -> Expr
replaceExprIdent ident replaceExpr givenExpr = case givenExpr of

    ExpBinOp binOp lExpr rExpr ->
        ExpBinOp binOp (replaceExprIdent ident replaceExpr lExpr)
                       (replaceExprIdent ident replaceExpr rExpr)

    ExpUnaryOp unOp expr -> ExpUnaryOp unOp (replaceExprIdent ident replaceExpr expr)

    ExpFunCall (FunCall name exprs) ->
        ExpFunCall (FunCall name $
            map (replaceExprIdent ident replaceExpr) exprs)

    ExpMethodCall (MethodCall cName method exprs) ->
        ExpMethodCall (MethodCall cName method $
            map (replaceExprIdent ident replaceExpr) exprs)

    ExpIdent expId -> if expId == ident then replaceExpr else ExpIdent expId

    ExpLit lit -> ExpLit lit


-- | inclusive MapWhile function, returns results which satisfy a condition
incMapWhile :: (b -> Bool) -> (a -> b) -> [a] -> [b]
incMapWhile cond f (x:xs) = if cond res then res : incMapWhile cond f xs else [res]
 where res = f x
incMapWhile _ _ [] = []


checkConst :: Expr -> GenState Literal
checkConst expr = case expr of
    (ExpLit l) -> return l
    _ -> lift $ Left "Expected constant expression"




-- | Attempts to reduce an expression as much as possible
-- | Returns an error string if evaluated expression
-- | is invalid or an identifier is not present in the given table
-- | otherwise returns the reduced expression
reduceExpr :: Expr -> GenState Expr
reduceExpr expr = do 
    cTable <- use constTable
    case expr of
        (ExpBinOp b e1 e2) -> do
            re1 <- reduceExpr e1
            re2 <- reduceExpr e2
            lift $ evaluateBinExpr b re1 re2

        (ExpUnaryOp u e) -> do
            reducedExpr <- reduceExpr e
            lift $ evaluateUnExpr u reducedExpr

        (ExpFunCall (FunCall s exps)) -> do
             rexps <- mapM reduceExpr exps
             return $ ExpFunCall (FunCall s rexps)

        (ExpMethodCall (MethodCall obj method args)) -> do
            rexps <- mapM reduceExpr args
            return $ ExpMethodCall (MethodCall obj method rexps)

        (ExpIdent i) -> return $ case M.lookup i cTable of
                            Just l -> ExpLit l
                            Nothing -> ExpIdent i

        (ExpLit l) -> return $ ExpLit l


-- | Attempts to evaluate a constant binary expression
evaluateBinExpr :: BinOps -> Expr -> Expr -> Either String Expr

-- | Binary operations with literals
evaluateBinExpr b (ExpLit l1) (ExpLit l2) = 
    case (b, l2) of
        -- Check for division by 0
        (Div, Number (Left 0)) -> divBy0Err 
        (Div, Number (Right 0.0)) -> divBy0Err
        _ -> binOpTable b l1 l2
  where divBy0Err = Left $ "Error, attempted to divide by 0"

evaluateBinExpr  b e1 e2 = return $ ExpBinOp b e1 e2
 

-- | Obtain binary operation to use with literal values
binOpTable :: BinOps -> Literal -> Literal -> Either String Expr
binOpTable b = case b of
    Add -> performBinNumOp (+) 
    Sub -> performBinNumOp (-) 
    Mul -> performBinNumOp (*)

    Div -> performDivision 

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


performBinNumOp :: (forall a. Num a => (a -> a -> a))  -> Literal -> Literal -> Either String Expr
performBinNumOp operation (Number (Left n1)) (Number (Left n2)) =
    Right $ ExpLit $ Number $ Left $ n1 `operation` n2

performBinNumOp operation (Number (Right n1))(Number (Right n2)) =
    Right $ ExpLit $ Number $ Right $ n1 `operation` n2

performBinNumOp _ _ _ = Left "Error expected a numeric value"


performDivision :: Literal -> Literal -> Either String Expr
performDivision (Number (Left n1)) (Number (Left n2)) = Right $ ExpLit $ Number (Left $ n1 `div` n2)
performDivision (Number (Right n1)) (Number (Right n2)) = Right $ ExpLit $ Number (Right $ n1 / n2)
performDivision _ _ = Left "Error, expected to divide either 2 integers or 2 doubles"

performBinIntOp :: (Integer -> Integer -> Integer)  -> Literal -> Literal -> Either String Expr
performBinIntOp operation (Number (Left n1)) (Number (Left n2)) =
    Right $ ExpLit $ Number $ Left $ n1 `operation` n2
performBinIntOp _ _ _ = Left "Error expected integer types"


performBinCompareOp :: (Literal -> Literal -> Bool) -> Literal -> Literal -> Either String Expr
performBinCompareOp operation l1 l2 =
    Right $ ExpLit $ Bl $ l1 `operation` l2


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



takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []
