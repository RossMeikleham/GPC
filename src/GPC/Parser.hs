{- GPC parser -}

module GPC.Parser(parseSource) where

import Control.Applicative hiding ((<|>), many, optional, empty)
import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import GPC.AST
import GPC.Lexer
import Control.Arrow

-- |Entire operator table
-- |Unary ops have higher precedence than binary ones
-- |so they are at the front of the list
operators = unaryOps ++ binaryOps

-- |Binary operators from highest to lowest precedence
binaryOps = [[binary "*"  Mul ,binary "/"  Div] --
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

 -- |All binary operators are infixed and left to right associative
 where binary n c = Infix (reservedOp n >> return (BinOp c)) AssocLeft

-- |Unary operators from highest to lowest precedence
unaryOps = [[unary "-" Neg, unary "!" Not, unary "~" BNot]]
 -- | All Unary operators are prefixed
 where unary n c = Prefix (reservedOp n >> return (UnaryOp c))


-- | Parse given source file, returns parse error string on
-- | failure otherwise returns the AST for the source
parseSource :: String -> Either String Program
parseSource = left show . parse program ""


-- | Parse entire source file
program :: Parser Program
program = Program <$> (whiteSpace *> topLevels) 


-- | Parse top level statements/definitions
topLevels :: Parser [TopLevel] 
topLevels =      try ((:) <$> topLevel <*> topLevels)
             <|> (eof >> return [])


-- | Parse Top Level definitions
topLevel :: Parser TopLevel
topLevel = function
        <|> try (TlAssign <$> assign)



-- | Parse Function definition
function :: Parser TopLevel
function = Func <$> parseType <*> parseIdent <*> fArgs <*> block
 where fArgs = parens args

    
-- | Parse Function arguments
args :: Parser [(Type, Ident)] 
args =  commaSep arg
 where arg :: Parser (Type,Ident) 
       arg = do 
           aType <- Type <$> typeT 
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
   <|> try (FunCallStmt <$> (funCall <* semi)) 
   <|> try (AssignStmt <$> assign)     
   <|> ((pure None) <* semi)


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
expr = buildExpressionParser operators expr'
 where expr' :: Parser Expr
       expr' = try (ExpFunCall <$> funCall) 
           <|> try (ExpIdent <$> parseIdent)
           <|> try (Lit   <$> literal)
           <|> parens expr


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
      <|> Num <$> num 


-- | Parse function call
funCall :: Parser FunCall
funCall = FunCall <$> ident <*> args'
    where args' = parens $ commaSep expr

parseIdent :: Parser Ident
parseIdent = Ident <$> ident

parseType :: Parser Type
parseType = Type <$> typeT

-- | Parse number
num :: Parser (Either Integer Double)
num = Right <$> try float
  <|> Left  <$> int
