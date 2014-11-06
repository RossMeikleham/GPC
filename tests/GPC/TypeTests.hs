-- Unit Tests for the type checker

module GPC.TypeTests(typeTests) where

import Test.HUnit
import GPC.AST
import GPC.TypeScopeChecker
import qualified Data.Map as M

expressions = [(ExpLit (Number (Left 20)))] 

expectedTypes = map (Type) ["int"]

vars = M.empty
ftable = M.empty

 
validTest :: Type -> Either String Type -> Test
validTest e a = TestCase (
 case a of
    Left err -> assertFailure err
    Right p -> assertEqual "" (show p) (show e))

typeTests :: Test
typeTests = test $ map (\(expected,expr) -> 
    validTest expected (getTypeExpr vars ftable expr)) (zip expectedTypes expressions) 
