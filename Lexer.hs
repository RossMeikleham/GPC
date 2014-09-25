{- GPC lexer -}

module Lexer 
    ( identifier
    , reserved
    , reservedOp
    , parens
    , integer
    , semi
    , whiteSpace
    , comma
    ) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


lexer = Token.makeTokenParser languageDef
 where languageDef = emptyDef {
       Token.commentStart  = "/*"
     , Token.commentEnd    = "*/"
     , Token.commentLine   = "//"
     , Token.nestedComments = True
     , Token.identStart    = letter
     , Token.identLetter   = alphaNum <|> oneOf "'_"
     , Token.reservedNames = ["int","double","bool","if","else"]
     , Token.reservedOpNames = ["+","-","*","/",">>","<<","!","!="
                               ,"==","^","&","|","||","&&"
                               ]
     , Token.caseSensitive = True
     }
             
             
identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer 
parens = Token.parens lexer
integer = Token.integer lexer
semi = Token.semi lexer 
whiteSpace = Token.whiteSpace lexer            
comma = Token.comma lexer

