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
exprOperators = operators (\n c -> (Prefix (reservedOp n >> return (ExpUnaryOp c))))
                          (\n c -> (Infix  (reservedOp n >> return (ExpBinOp c)) AssocLeft))

-- |Unary operators have higher precedence than binary ones
operators un bin = (unaryOps un) ++ (binaryOps bin)

-- |Binary operators from highest to lowest precedence
binaryOps :: ([Char] -> BinOps -> Operator s u m a) -> [[Operator s u m a]]
binaryOps binary = [[binary "*"  Mul ,binary "/"  Div, binary "%" Mod] --
            ,[binary "+"  Add, binary "-"  Sub]
            ,[binary "<<" ShiftL ,binary ">>" ShiftR] 
            ,[binary "<"  Less ,binary "<=" LessEq 
             ,binary ">"  Greater ,binary ">=" GreaterEq]
            ,[binary "==" Equals ,binary "!=" NEquals]
            ,[binary "&"  BAnd]
            ,[binary "^"  BXor]
            ,[binary "|"  BOr]
            ,[binary "&&" And]
            ,[binary "||" Or]
            ]


-- |Unary operators from highest to lowest precedence
unaryOps :: ([Char] -> UnaryOps -> Operator s u m a) -> [[Operator s u m a]]
unaryOps unary = [[unary "-" Neg, unary "!" Not, unary "~" BNot, unary "*" Deref]]


-- | Parse given source file, returns parse error string on
-- | failure otherwise returns the AST for the source
parseSource :: String -> Either String Program
parseSource = left show . parse program ""


-- | Parse entire source file
program :: Parser Program
program = Program <$> (whiteSpace *> topLevels) 


-- | Parse top level statements/definitions
topLevels :: Parser [TopLevel] 
topLevels = try ((:) <$> topLevel <*> topLevels) 
        <|> try (do 
             aAssign <- arrayAssign
             let aAssign' = map TLAssign aAssign
             tls <- topLevels
             return $ aAssign' ++ tls)
         <|> (eof >> return [])



-- | Parse Top Level definitions
topLevel :: Parser TopLevel
topLevel = try function
        <|> try (TLAssign <$> assign)
        <|> try (TLObjs <$> objs)
        <|> (TLConstructObjs <$> constructObjs)


-- | Parse C++ Object definitions
objs :: Parser Objects
objs = do 
    (libName, className) <- parseClass
    var <- parseVar
    _ <- semi
    return $ Objects libName className var


constructObjs :: Parser ConstructObjs
constructObjs = do 
    var <- parseVar
    reservedOp "="
    (libName, className) <- parseClass
    exprs <- parens $ commaSep expr
    _ <- semi 
    return $ ConstructObjs var libName className exprs 

-- | Parse Class 
parseClass :: Parser (LibName, ClassName)
parseClass = do 
    libName <- parseIdent
    reservedOp "::"
    className <- parseIdent
    return (libName, className)


-- | Parse Function definition
function :: Parser TopLevel
function = Func <$> parseType <*> parseIdent <*> fArgs <*> block
 where fArgs = parens args

    
-- | Parse Function arguments
args :: Parser [(Type, Ident)] 
args =  commaSep arg
 where arg :: Parser (Type,Ident) 
       arg = do 
           aType <- parseType
           aName <- Ident <$> ident
           return (aType, aName)


-- | Parse a block of statements encased in braces
block :: Parser BlockStmt
block = BlockStmt <$> braces stmts


-- | Parse multiple statements
stmts :: Parser [Stmt]
stmts = many1 stmt


-- | Parse individual statement
stmt :: Parser Stmt
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
ifStmt :: Parser Stmt
ifStmt = try (IfElse <$> parseIf <*> stmt <*> (reserved "else" *> stmt))
     <|>      If     <$> parseIf <*> stmt
 where parseIf = (reserved "if" *> parens expr)


-- | Parse block to be executed sequentially
seqBlock :: Parser Stmt
seqBlock = Seq <$> (reserved "seq" *> block)


-- | Parse block to be executed in parallel
parBlock :: Parser Stmt
parBlock = BStmt <$> (reserved "par" *> block)


-- | Parse Expression
expr :: Parser Expr
expr = buildExpressionParser exprOperators expr'
 where expr' :: Parser Expr
       expr' = try (ExpFunCall <$> funCall) 
           <|> try (ExpMethodCall <$> methodCall)
           <|> try (ExpIdent <$> parseIdent)
           <|> try (ExpLit   <$> literal)
           <|> parens expr

-- | Parse Static Array declaration
-- | type name[] = {elem1, elem2, ... elemn};
arrayAssign :: Parser [Assign]
arrayAssign = do
    aType <- parseType
    arrayName <- parseIdent
    reservedOp "["
    reservedOp "]"
    reservedOp "="
    reservedOp "{"
    exprs <- commaSep expr
    reservedOp "}"
    _ <- semi
    let arrPtr = Assign (PointerType aType) arrayName $ ExpPointer (Pointer arrayName 0)
    let arrElems = map (\(e, n) -> 
                Assign (aType) (catIdent n arrayName) e) $ zip exprs [0,1..]
    return $ arrElems ++ [arrPtr]
 where 
    -- Prefix integer onto Identifier
    catIdent :: Int -> Ident -> Ident
    catIdent n (Ident a) = Ident (show n ++ a)


-- | Parse variable assignment
assign :: Parser Assign
assign = Assign <$> parseType <*> 
                    parseIdent <* parseCh '=' <*> 
                    (expr <* semi)


-- | Parse literal
literal :: Parser Literal
literal = Ch  <$> ch 
      <|> Str <$> str 
      <|> Bl  <$> bool 
      <|> Number <$> num 


-- | Parse for loop
forLoop :: Parser Stmt
forLoop = 
    ForLoop <$> (reserved "for" *> reservedOp "(" *> reserved "int" *>  parseIdent) -- Identifier to use
            <*> (reservedOp "=" *> expr) -- Start
            <*> (semi *> expr)  -- Stop
            <*> (semi *> expr <* reservedOp ")") -- Step
            <*> block
   
-- | Parse function call
funCall :: Parser FunCall
funCall = FunCall <$> parseIdent <*> args'
    where args' = parens $ commaSep expr

-- | Parse method call
methodCall :: Parser MethodCall
methodCall = try (MethodCall <$> parseIdent <*> (reservedOp "." *> parseIdent) <*> args')
         <|> do 
            (VarArrayElem name _) <- parseVar
            reservedOp "."
            method <- parseIdent
            args'' <- args'
            return $ MethodCall name method args''
            
    where args' = parens $ commaSep expr


-- | Parse varaible
parseVar :: Parser Var
parseVar = try (VarArrayElem <$> parseIdent <*> (brackets expr)) 
       <|> VarIdent <$> parseIdent

-- | Parse identifier
parseIdent :: Parser Ident
parseIdent = Ident <$> ident

-- | Parse types
parseType :: Parser Type
parseType = do
    baseType <- NormalType <$> typeT
    ptrs <- many getPointer 
    return $ foldr (\ ptr cur -> (ptr cur)) baseType ptrs
 where
    getPointer :: Parser (Type -> Type)
    getPointer = do
        reservedOp "*" 
        return PointerType 


-- | Parse number
num :: Parser (Either Integer Double)
num = Right <$> try float
  <|> Left  <$> int
