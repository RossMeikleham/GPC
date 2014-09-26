{- GPC lexer -}

{-# LANGUAGE NoMonomorphismRestriction #-}

module GPC.Lexer 
    ( ident
    , reserved
    , reservedOp
    , parens
    , int
    , ch
    , str
    , float
    , semi
    , whiteSpace
    , comma
    , braces
    , typeT
    , commaSep
    , semiSep
    , parseCh
    ) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Control.Applicative hiding ((<|>), many, optional, empty)

reservedTypes = ["int", "double", "bool","void"]
otherReserved = ["if", "else"]
boolValues = ["true", "false"]

languageDef = emptyDef {
       Token.commentStart  = "/*"
     , Token.commentEnd    = "*/"
     , Token.commentLine   = "//"
     , Token.nestedComments = True
     , Token.identStart    = letter
     , Token.identLetter   = alphaNum <|> oneOf "'_"
     , Token.reservedNames =   otherReserved ++ reservedTypes
     , Token.reservedOpNames = ["+","-","*","/",">>","<<","!","!="
                               ,"==","^","&","|","||","&&"
                               ]
     , Token.caseSensitive = True
     }
lexer = Token.makeTokenParser languageDef
             
             
ident = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer 
parens = Token.parens lexer
semi = Token.semi lexer 
whiteSpace = Token.whiteSpace lexer            
comma = Token.comma lexer
braces = Token.braces lexer
commaSep = Token.commaSep lexer
semiSep = Token.semiSep lexer

-- Parse specific character
parseCh c = reserved [c]
-- Parse specific string
parseStr s = reserved s



ch = Token.charLiteral lexer
int = Token.hexadecimal lexer <|> Token.octal lexer <|> Token.integer lexer 
float = Token.float lexer
str = Token.stringLiteral lexer

-- |Parse boolean values
bool = parseStr "true" *> pure True
   <|> parseStr "false" *> pure False 


-- |When we need to use built in types
typeT = Token.identifier typeLexer
 where typeLexer = Token.makeTokenParser languageDef 
                   {Token.reservedNames = otherReserved}








