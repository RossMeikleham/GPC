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

-- Generate code for statements
genStmt :: Stmt -> String
genStmt s = "placeholder"


label :: String -> String -> String
label n s = "'" ++ (parens $ "label " ++ s)

letExp :: String -> String
letExp s =  parens $ "let " ++ s 

begin :: String -> String
begin s = parens $ "begin " ++ s


parens :: String -> String
parens s = "(" ++ s ++ ")" 
