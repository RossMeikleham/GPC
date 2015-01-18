{- GPC parser -}

module GPC.Parser(parseSource) where

import Control.Applicative hiding ((<|>), many, optional, empty)
import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import GPC.AST
import GPC.Lexer
import Control.Arrow


{- Operator Tables -}

-- | Need operators to evaluate ordinary expressions and constant expressions
exprOperators pos = operators pos (\n c -> (Prefix (reservedOp n >> return (ExpUnaryOp c))))
                              (\n c -> (Infix  (reservedOp n >> return (ExpBinOp c)) AssocLeft))

-- |Unary operators have higher precedence than binary ones
--
operators pos un bin = (unaryOps pos un) ++ (binaryOps pos bin)


-- |Binary operators from highest to lowest precedence
binaryOps :: SrcPos -> ([Char] -> BinOps SrcPos -> Operator s u m a) -> [[Operator s u m a]]
binaryOps pos binary =     
    [[binary "*"  (Mul pos) ,binary "/"  (Div pos), binary "%" (Mod pos)]
     ,[binary "+"  (Add pos), binary "-"  (Sub pos)]
     ,[binary "<<" (ShiftL pos) ,binary ">>" (ShiftR pos)] 
     ,[binary "<"  (Less pos),binary "<=" (LessEq pos)]
     ,[binary ">"  (Greater pos) ,binary ">=" (GreaterEq pos)]
     ,[binary "==" (Equals pos) ,binary "!=" (NEquals pos)]
     ,[binary "&"  (BAnd pos)]
     ,[binary "^"  (BXor pos)]
     ,[binary "|"  (BOr pos)]
     ,[binary "&&" (And pos)]
     ,[binary "||" (Or pos)]
    ]

-- |Unary operators from highest to lowest precedence
unaryOps :: SrcPos -> ([Char] -> UnaryOps SrcPos -> Operator s u m a) -> [[Operator s u m a]]
unaryOps pos unary = [[unary "-" (Neg pos), unary "!" (Not pos), unary "~" (BNot pos)]]

-- | Parse given source file, returns parse error string on
-- | failure otherwise returns the AST for the source
parseSource :: String -> Either String (Program SrcPos)
parseSource = left show . parse program ""


-- | Parse entire source file
program :: Parser (Program SrcPos)
program = Program <$> (whiteSpace *> topLevels) 


-- | Parse top level statements/definitions
topLevels :: Parser [TopLevel SrcPos] 
topLevels = ((:) <$> topLevel <*> topLevels) 
         <|> (eof >> return [])



-- | Parse Top Level definitions
topLevel :: Parser (TopLevel SrcPos)
topLevel = try function
        <|> try (TLAssign <$> assign)
        <|> try (TLObjs <$> objs)
        <|> (TLConstructObjs <$> constructObjs)


-- | Parse C++ Object definitions
objs :: Parser (Objects SrcPos)
objs = do 
    ns <- sepBy2 parseIdent $ reservedOp "::"
    var <- parseVar
    _ <- semi
    return $ Objects ns var
  where sepBy2 seg sep = do
            x <- seg
            _ <- sep
            xs <- sepBy1 seg sep
            return (x:xs)  


constructObjs :: Parser (ConstructObjs SrcPos)
constructObjs = do 
    var <- parseVar
    reservedOp "="
    ns <- sepBy1 parseIdent $ reservedOp "::"
    exprs <- parens $ commaSep expr
    _ <- semi 
    return $ ConstructObjs ns var exprs 


-- | Parse Function definition
function :: Parser (TopLevel SrcPos)
function = Func <$> parseType <*> parseIdent <*> fArgs <*> block
 where fArgs = parens args

    
-- | Parse Function arguments
args :: Parser [(Type SrcPos, Ident SrcPos)] 
args =  commaSep arg
 where arg :: Parser (Type SrcPos, Ident SrcPos) 
       arg = do 
           aType <- parseType
           aName <- parseIdent
           return (aType, aName)


-- | Parse a block of statements encased in braces
block :: Parser (BlockStmt SrcPos)
block = BlockStmt <$> braces stmts


-- | Parse multiple statements
stmts :: Parser [Stmt SrcPos]
stmts = many stmt


-- | Parse individual statement
stmt :: Parser (Stmt SrcPos)
stmt = try (Return <$> (reserved "return" *> (expr <* semi)))
   <|> try (BStmt <$> block)
   <|> try ifStmt
   <|> try seqBlock
   <|> try parBlock
   <|> try forLoop 
   <|> try (FunCallStmt <$> (funCall <* semi)) 
   <|> try (MethodStmt <$> (methodCall  <* semi))
   <|> try (AssignStmt <$> assign)     


-- | Parse if statement
ifStmt :: Parser (Stmt SrcPos)
ifStmt = try (IfElse <$> parseIf <*> stmt <*> (reserved "else" *> stmt))
     <|>      If     <$> parseIf <*> stmt
 where parseIf = (reserved "if" *> parens expr)


-- | Parse block to be executed sequentially
seqBlock :: Parser (Stmt SrcPos)
seqBlock = Seq <$> (reserved "seq" *> block)


-- | Parse block to be executed in parallel
parBlock :: Parser (Stmt SrcPos)
parBlock = BStmt <$> (reserved "par" *> block)


-- | Parse Expression
expr :: Parser (Expr SrcPos)
expr = do
    pos <- getPos
    buildExpressionParser (exprOperators pos) expr'
 where expr' :: Parser (Expr SrcPos)
       expr' = try (ExpFunCall <$> funCall) 
           <|> try (ExpMethodCall <$> methodCall)
           <|> try (ExpIdent <$> parseIdent)
           <|> try (ExpLit   <$> literal)
           <|> parens expr

-- | Parse variable assignment
assign :: Parser (Assign SrcPos)
assign = Assign <$> parseType <*> 
                    parseIdent <* parseCh '=' <*> 
                    (expr <* semi)


-- | Parse literal
literal :: Parser (Literal SrcPos)
literal = Ch     <$> getPos <*> ch 
      <|> Str    <$> getPos <*> str   
      <|> Bl     <$> getPos <*> bool 
      <|> Number <$> getPos <*> num 


-- | Parse for loop
forLoop :: Parser (Stmt SrcPos)
forLoop = 
    ForLoop <$> (reserved "for" *> reservedOp "(" *> reserved "int" *>  parseIdent) -- Identifier to use
            <*> (reservedOp "=" *> expr) -- Start
            <*> (semi *> expr)  -- Stop
            <*> (semi *> expr <* reservedOp ")") -- Step
            <*> block
   
-- | Parse function call
funCall :: Parser (FunCall SrcPos)
funCall = FunCall <$> parseIdent <*> args'
    where args' = parens $ commaSep expr

-- | Parse method call
methodCall :: Parser (MethodCall SrcPos)
methodCall = do
            var <- parseVar
            reservedOp "."
            method <- parseIdent
            args'' <- args'
            return $ MethodCall var method args''
            
    where args' = parens $ commaSep expr


-- | Parse varaible
parseVar :: Parser (Var SrcPos)
parseVar = try (VarArrayElem <$> parseIdent <*> (brackets expr)) 
       <|> VarIdent <$> parseIdent

-- | Parse identifier
parseIdent :: Parser (Ident SrcPos)
parseIdent = Ident <$> getPos <*> ident

-- | Parse types
parseType :: Parser (Type SrcPos)
parseType = do
    baseType <- NormalType <$> getPos <*> pure False <*> typeT
    ptrs <- many getPointer 
    return $ foldr (\ ptr cur -> (ptr cur)) baseType ptrs
 where
    getPointer :: Parser (Type SrcPos -> Type SrcPos)
    getPointer = (char '*' >> whiteSpace >> return PointerType)
            

-- | Parse number
num :: Parser (Either Integer Double)
num = Right <$> try float
  <|> Left  <$> int


getPos :: Parser SrcPos
getPos = do
    sp <- getPosition
    return $ SrcPos (sourceLine sp) (sourceColumn sp)


