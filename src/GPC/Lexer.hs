{- GPC lexer -}

{-# LANGUAGE NoMonomorphismRestriction #-}

module GPC.Lexer 
    ( ident
    , reserved
    , reservedOp
    , parens
    , integer
    , semi
    , whiteSpace
    , comma
    , braces
    , typeT
    , commaSep
    , semiSep
    ) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Control.Applicative hiding ((<|>), many, optional, empty)

reservedTypes = ["int", "double", "bool","void"]
otherReserved = ["if", "else"]

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
integer = Token.integer lexer
semi = Token.semi lexer 
whiteSpace = Token.whiteSpace lexer            
comma = Token.comma lexer
braces = Token.braces lexer
commaSep = Token.commaSep lexer
semiSep = Token.semiSep lexer

-- |When we need to use built in types
typeT = Token.identifier typeLexer
 where typeLexer = Token.makeTokenParser languageDef 
                   {Token.reservedNames = otherReserved}

