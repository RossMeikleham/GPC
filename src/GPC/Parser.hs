{- GPC parser -}

module GPC.Parser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
--import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Error;
import Control.Applicative hiding ((<|>), many, optional, empty)


import GPC.AST
import GPC.Lexer


run :: Show a => Parser a -> String -> IO ()
run p input
        = case parse p "" input of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print x


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
            

-- | Parse expression
expr :: Parser Expr
expr = pure $ Literal "Hello"  


-- | Parse type declaration
decl :: Parser Stmt
decl = Decl <$> typeT <*> ident <* parseCh '=' <*> expr


-- | Parse literal
--literal :: Parser Literal
--literal = Num   

-- | Parse number



