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
    )

import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token



lexer = Token.makeTokenParser languageDef
 where languageDef =
     { Token.commentStart  = "/*"
     , Token.commentEnd    = "*/"
     , Token.commentLine   = "//"
     , Token.nestedComments = True
     , Token.identStart    = letter
     , Token.identLetter   = alphaNum
     , Token.reservedNames = ["int","double","bool"]
                             
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

