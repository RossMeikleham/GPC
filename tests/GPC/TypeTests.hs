-- Unit Tests for the type checker

module GPC.TypeTests(typeTests) where

import Test.HUnit
import GPC.AST
import GPC.TypeScopeChecker
import qualified Data.Map as M

doubleConst a = ExpLit $ Number $ Right a
intConst a = ExpLit $ Number $ Left a
strConst = ExpLit . Str 

expressions = [intConst 20
              ,(ExpBinOp Less (intConst 10) (ExpIdent $ Ident "a"))
              ,(ExpFunCall $ FunCall (Ident "fun1") 
                [intConst 10, ExpBinOp Add (intConst 20) (ExpIdent $ Ident "a")])
              ,strConst "hi"
              ,(ExpUnaryOp BNot (ExpIdent $ Ident "b"))
              ] 

failExpressions = [(ExpBinOp Less (intConst 10) (doubleConst 20.0))]

expectedTypes = map (Type) ["int"
                           ,"bool"
                           ,"int"
                           ,"string"
                           ,"bool"
                           ]

vars = M.fromList $ map (\(a,b) -> (Ident a, Type b)) 
              [("a", "int")
              ,("b", "bool")
              ,("c", "double")
              ]
ftable = M.fromList [(Ident "fun1", (Type "int" , map Type ["int", "int"]))
                     ]
ctable = M.fromList [("a", Number (Left 6))
                    ,("b", Bl True)
                    ,("c", Number (Right 7.0))
                    ]

 
validTest :: Type -> Either String Type -> Test
validTest e a = TestCase (
 case a of
    Left err -> assertFailure err
    Right p -> assertEqual "" (show p) (show e))

typeTests :: Test
typeTests = test $ map (\(expected,expr) -> 
    validTest expected (getTypeExpr vars ftable expr)) (zip expectedTypes expressions) 
