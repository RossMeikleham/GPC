-- Unit Tests for the type checker

module GPC.TypeTests(typeTests) where

import Test.HUnit
import GPC.AST
import GPC.TypeScopeChecker
import Control.Monad
import qualified Data.Map as M
import Data.Either

doubleConst a = ExpLit $ Number $ Right a
intConst a = ExpLit $ Number $ Left a
strConst = ExpLit . Str 

expressions = [intConst 20 -- ^ Check constant integer
              ,(ExpBinOp Less (intConst 10) (ExpIdent $ Ident "a")) -- ^ Check binary expression
              ,(ExpFunCall $ FunCall (Ident "fun1")  -- ^ Check Function call expression
                [intConst 10, ExpBinOp Add (intConst 20) (ExpIdent $ Ident "a")]) 
              ,strConst "hi" -- ^ Check string literal
              ,(ExpUnaryOp BNot (ExpIdent $ Ident "b")) -- ^ Check unary expression
              ] 


expectedTypes = map (Type) ["int"
                           ,"bool"
                           ,"int"
                           ,"string"
                           ,"bool"
                           ,"int"
                           ]

-- ^ Invalid expressions which are expected to give an error message
failExpressions = [(ExpBinOp Less (intConst 10) (doubleConst 20.0))
                  -- Check functions called with more args gives error
                  ,(ExpFunCall $ (FunCall (Ident "fun1") 
                    [(intConst 1), (intConst 1), (intConst 1)]))
                  ,(ExpFunCall $ (FunCall (Ident "fun1") [(intConst 1)]))
                  ]


-- Quick test, move to own HUnit module later
injectExpressions = [(ExpBinOp Mul (ExpIdent (Ident "a")) (ExpLit (Number (Left 4))))]

expectedAfterInject = [intConst 24]


vars = M.fromList $ map (\(a,b) -> (Ident a, Type b)) 
              [("a", "int")
              ,("b", "bool")
              ,("c", "double")
              ]
ftable = M.fromList [(Ident "fun1", (Type "int" , map Type ["int", "int"]))
                     ]
ctable = M.fromList [(Ident "a", Number (Left 6))
                    ,(Ident "b", Bl True)
                    ,(Ident "c", Number (Right 7.0))
                    ]

 
validTest :: Type -> Either String Type -> Test
validTest e a = TestCase (
 case a of
    Left err -> assertFailure err
    Right p -> assertEqual "" (show p) (show e))

validInject :: Expr -> Either String Expr -> Test
validInject e a = TestCase (
 case a of
    Left err -> assertFailure err
    Right p -> assertEqual "" (show p) (show e))

invalidTest :: Either String Type -> Test
invalidTest a = TestCase (
    unless (isLeft a) $ 
    assertFailure "Expected test to fail")

typeTests :: Test
typeTests = test $ (map (\(expected,expr) -> 
    validTest expected (getTypeExpr vars ftable expr)) (zip expectedTypes expressions)) ++
    (map (invalidTest . (getTypeExpr vars ftable) ) failExpressions) ++
    (map (\(expected, expr) -> 
        validInject expected (reduceExpr vars $ injectConstants ctable expr)) 
        (zip expectedAfterInject injectExpressions)
    )


