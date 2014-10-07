{- Generate GPIR code from AST -}

module GPC.CodeGen (genCode) where

import Text.PrettyPrint.Leijen hiding (Str)
import GPC.AST

nestLevel = 4 -- Number of spaces to nest

concatMapDocs :: (a -> Doc) -> [a] -> Doc
concatMapDocs f ds = hcat $ map f ds 


-- Generate GPIR code from Program AST
genCode :: Program -> String
genCode (Program xs) = show doc
 where doc = concatMapDocs genTopLevel xs


-- Generate code from Top Level statements
genTopLevel :: TopLevel -> Doc
genTopLevel (TlStmt ss) = genStmt ss
genTopLevel (Func _ n args rest) = case args of
    -- Simple begin label for no argument functions
    [] -> letExp $ label (text n) $ begin $ concatMapDocs genStmt rest
    -- Need to generate lambda for functions with arguments
    _  -> letExp $ label (text n) $ lambda (map text vars) $ 
            begin $ concatMapDocs genStmt rest
 where vars = map snd args


-- |Generate code for statements
genStmt :: Stmt -> Doc
genStmt (Decl _ name ex) = assign (text name) $ genExpr ex
genStmt (Seq s) = letExp $ concatMapDocs genStmt s
genStmt (Par ss) = parens' $ concatMapDocs genStmt ss
genStmt (Exp e) = genExpr e
genStmt (If e s) =  ifStmt e s
genStmt (IfElse e s1 s2) = ifElseStmt e s1 s2 
genStmt (Return e) = genReturn $ genExpr e 
genStmt (BlockStmt ss) = concatMapDocs genStmt ss
genStmt (None) = text ""

        
-- |Generate code for expressions
genExpr :: Expr -> Doc
genExpr (Lit l) = genLit l
genExpr (FunCall n args) = apply (text n) $ concatMapDocs genExpr args
genExpr (Ident s) = text s 
-- | Generate Literal 
genLit :: Literal -> Doc
genLit l =  (char '\'') <> text (genLit' l)
 where   
    genLit' :: Literal -> String
    genLit' (Str s) = "\"" ++ s ++ "\""
    genLit' (Ch  c) = show c
    genLit' (Bl  b) = show b
    genLit' (Num n) = case n of
        Left i -> show i
        Right d -> show d


-- | Generate apply
apply :: Doc -> Doc -> Doc
apply n s = deferParens $ text "apply" <+> n <+> s

ifStmt :: Expr -> Stmt -> Doc
ifStmt cond ex = parens' $ text "if" <+> (parens $ genExpr cond) <> thenStmt 
 where thenStmt = deferParens $ genStmt ex 

ifElseStmt :: Expr -> Stmt -> Stmt -> Doc
ifElseStmt cond ex elStmt= 
    parens' $ text "if" <+> (parens $ genExpr cond) <> thenStmt <> elseStmt
 where 
    thenStmt = deferParens $ genStmt ex 
    elseStmt = deferParens $ genStmt elStmt

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
