{- Generate GPIR code from AST -}

module GPC.CodeGen (genCode) where

import GPC.AST

-- Generate GPIR code from Program AST
genCode :: Program -> String
genCode (Program xs) = concatMap genTopLevel xs


-- Generate code from Top Level statements
genTopLevel :: TopLevel -> String
genTopLevel (TlStmt ss) = genStmt ss
genTopLevel (Func _ n args rest) = case args of
    -- Simple begin label for no argument functions
    [] -> letExp $ label n $ begin $ concatMap genStmt rest
    -- Need to generate lambda for functions with arguments
    _  -> "placeholder"

-- |Generate code for statements
genStmt :: Stmt -> String
genStmt (Decl _ name ex) = assign name $ genExpr ex
genStmt (Seq s) = letExp $ concatMap genStmt s
genStmt (Par ss) = parens $ concatMap genStmt ss
genStmt (Exp e) = genExpr e
genStmt (If e s) =  ifStmt e s
genStmt (IfElse e s1 s2) = ifElseStmt e s1 s2 
genStmt (Return e) = genReturn $ genExpr e 
genStmt (BlockStmt ss) = concatMap genStmt ss
genStmt (None) = ""
        
-- |Generate code for expressions
genExpr :: Expr -> String
genExpr (Lit l) = genLit l
genExpr (FunCall n args) = apply n $ concatMap genExpr args
genExpr (Ident s) = s

-- | Generate Literal 
genLit :: Literal -> String
genLit l = "`" ++ genLit' l
 where   
    genLit' :: Literal -> String 
    genLit' (Str s) = s
    genLit' (Ch  c) = show c
    genLit' (Bl  b) = show b
    genLit' (Num n) = case n of
        Left i -> show i
        Right d -> show d


-- | Generate apply
apply :: String -> String -> String
apply n s = deferParens $ "apply " ++ n  ++ " " ++ s

ifStmt :: Expr -> Stmt -> String 
ifStmt cond ex = parens $ "if " ++ (parens $ genExpr cond) ++ thenStmt 
 where thenStmt = deferParens $ genStmt ex 

ifElseStmt :: Expr -> Stmt -> Stmt -> String
ifElseStmt cond ex elStmt= ifStmt cond ex ++ elseStmt
 where elseStmt = deferParens $ genStmt elStmt

genReturn :: String -> String
genReturn s = deferParens $ "return " ++ s

-- | Assign expression to variable
assign :: String -> String -> String
assign n s = deferParens $ "assign " ++ n ++ " " ++ s

label :: String -> String -> String
label n s = deferParens $ "label " ++ n ++ " " ++  s

letExp :: String -> String
letExp s =  parens $ "let " ++ s 

begin :: String -> String
begin s = parens $ "begin " ++ s


parens :: String -> String
parens s = "(" ++ s ++ ")" 

-- |Parens to defer evaluation
deferParens :: String -> String
deferParens s = "'(" ++ s ++ ")"

