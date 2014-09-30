{- GPC parser -}

module GPC.Parser(parseSource) where

import Control.Applicative hiding ((<|>), many, optional, empty)
import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import GPC.AST
import GPC.Lexer

-- |Combine all operators into one table
-- |Unary ops all have higher precedence than binary ones
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
 where unary n c = Prefix (reservedOp n >> return (UnaryOp c))


-- | Parse given source file, returns parse error string on
-- | failure otherwise returns the AST for the source
parseSource :: String -> Either String Program
parseSource input = either (Left . show) (Right) $ parse program "" input 


-- | Parse entire source file
program :: Parser Program
program = Program <$> (whiteSpace *> topLevels) 


-- | Parse top level statements/definitions
topLevels :: Parser [TopLevel] 
topLevels =      try ((:) <$> topLevel <*> topLevels)
             <|> (eof >> return [])


-- | Parse Top Level definitions
topLevel :: Parser TopLevel
topLevel = try function 
       <|> TlStmt <$> stmt


-- | Parse Function definition
function :: Parser TopLevel
function = Func <$> typeT <*> ident <*> fArgs <*> block
 where fArgs = parens args

    
-- | Parse Function arguments
args :: Parser [(String, String)] 
args =  commaSep arg
 where arg :: Parser (String,String) 
       arg = do 
           aType <- typeT 
           aName <- ident
           return (aType, aName)


-- | Parse a block of statements encased in braces
block :: Parser [Stmt]
block = braces stmts


-- | Parse multiple statements
stmts :: Parser [Stmt]
stmts = many1 stmt


-- | Parse individual statement
stmt :: Parser Stmt
stmt = try (stmt' <* semi) <|> ((pure None) <* semi)
 where stmt' :: Parser Stmt
       stmt' = try decl
           <|> (Exp <$> expr)
            
expr :: Parser Expr
expr = buildExpressionParser operators expr'
 where expr' :: Parser Expr
       expr' = try (Ident <$> ident)
          <|> try (Lit   <$> literal)
          <|> parens expr


-- | Parse type declaration
decl :: Parser Stmt
decl = Decl <$> typeT <*> ident <* parseCh '=' <*> expr


-- | Parse literal
literal :: Parser Literal
literal = Ch  <$> ch 
      <|> Str <$> str 
      <|> Bl  <$> bool 
      <|> Num <$> num 


-- | Parse number
num :: Parser (Either Integer Double)
num = Right <$> try float
  <|> Left  <$> int
