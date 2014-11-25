{- Generate GPIR code from AST 
 - this module is temporary for playing around with the language
 - and pretty printers. In the final version the AST will go
 - through transformations and type/scope checking before
 - reaching this stage-}

{-# LANGUAGE TemplateHaskell #-}

module GPC.CodeGen (genCode) where

import Data.Char
import Text.PrettyPrint.Leijen
import Control.Monad.State.Lazy
import qualified Data.Map as M
import GPC.AST
import GPC.GPIRAST
{-
type VarTable = M.Map Ident Type
type ConstVarTable = M.Map Ident Literal
type FunTable = M.Map Ident SymbolTree

data CodeGen = {
   _funTable :: FunTable  -- ^ Store symbol tree for functions
}

-- Create lenses to access Block fields easier
makeLenses ''CodeGen

type GenState a = StateT CodeGen (Either String) a 


genGPIR :: Program -> SymbolTree
genGPIR (Program xs) = genTopLevel xs

genTopLevel :: [TopLevel] -> SymbolTree
-}

nestLevel = 4 -- |Number of spaces to nest

-- | Concatonate a list of Docs with hcat
concatMapDocs :: (a -> Doc) -> [a] -> Doc
concatMapDocs f ds = hcat $ map f ds 


-- |Generate GPIR code from Program AST
genCode :: Program -> String
genCode (Program xs) = show doc
 where doc = concatMapDocs genTopLevel xs


-- Generate code from Top Level statements
genTopLevel :: TopLevel -> Doc
--genTopLevel (TlStmt ss) = genStmt ss
genTopLevel (Func _ (Ident n) args (BlockStmt rest)) = case args of
    -- Simple begin label for no argument functions
    [] -> letExp $ label (text n) $ begin $ concatMapDocs genStmt rest
    -- Need to generate lambda for functions with arguments
    _  -> letExp $ label (text n) $ lambda (map text vars) $ 
            begin $ concatMapDocs genStmt rest
 where vars = map (\(Type t, Ident i) -> i) args
 

-- |Generate code for statements
genStmt :: Stmt -> Doc
genStmt (FunCallStmt (FunCall (Ident n) args)) = 
    apply (text n) $ foldl (<+>) empty $ map genExpr args
genStmt (AssignStmt (Assign _ (Ident name) ex)) = 
    assign (text name) $ genExpr ex
genStmt (Seq (BlockStmt s)) = letExp $ concatMapDocs genStmt s
genStmt (If e s) =  ifStmt e s
genStmt (IfElse e s1 s2) = ifElseStmt e s1 s2 
genStmt (Return e) = genReturn $ genExpr e 
genStmt (BStmt (BlockStmt ss)) = concatMapDocs genStmt ss
        
-- |Generate code for expressions
genExpr :: Expr -> Doc
genExpr (ExpLit l) = genLit l
genExpr (ExpFunCall (FunCall (Ident n) args)) = 
    apply (text n) $ foldl (<+>) empty $ map genExpr args
genExpr (ExpIdent (Ident s)) = text s 


-- | Generate Literal 
genLit :: Literal -> Doc
genLit l =  (char '\'') <> text (genLit' l)
 where   
    genLit' :: Literal -> String
    genLit' (Str s) = "\"" ++ s ++ "\""
    genLit' (Ch  c) = show c
    genLit' (Bl  b) = map toLower (show b)
    genLit' (Number n) = case n of
        Left i -> show i
        Right d -> show d


-- | Generate apply
apply :: Doc -> Doc -> Doc
apply n s = deferParens $ text "apply" <+> n <> s

ifStmt :: Expr -> Stmt -> Doc
ifStmt cond ex = parens' $ text "if" <+> (parens $ genExpr cond) <> thenStmt 
 where thenStmt = deferParens $ genStmt ex 

ifElseStmt :: Expr -> Stmt -> Stmt -> Doc
ifElseStmt cond ex elStmt= 
    parens' $ text "if" <+> (parens $ genExpr cond) <> thenStmt <> elseStmt
 where 
    thenStmt = deferParens $ genStmt ex 
    elseStmt = deferParens $ genStmt elStmt

-- | Generate return
genReturn :: Doc -> Doc
genReturn s = deferParens $ text "return" <+> s

-- | Generate lambda
lambda :: [Doc] -> Doc -> Doc
lambda xs s = parens $ text "lambda" <+> vars <+> s
 where vars = foldl1 (<+>) $ map (\m -> char '\'' <> m) xs

-- | Assign expression to variable
assign :: Doc -> Doc -> Doc
assign n s = deferParens $ text "assign" <+> n <+> s

-- | Generate Label
label :: Doc -> Doc -> Doc
label n s = deferParens $ text "label" <+> n <+> s

-- | Generate Expression
letExp :: Doc -> Doc
letExp s =  parens' $ text "let" <+> s 

-- | Generate begin, returns value of last expression
begin :: Doc -> Doc
begin s = parens' $ text "begin" <+> s

-- |Parens to defer evaluation
deferParens :: Doc -> Doc
deferParens s = nest' 4 $ char '\'' <> parens s

parens' x = nest' 4 $ parens x

nest' :: Int -> Doc -> Doc
nest' n d = text "" <$> nest n d
