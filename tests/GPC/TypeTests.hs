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
isRight' = either (const False) (const True)

intTypeNK = NormalType False "int"
intTypeK  = NormalType True  "int"
boolTypeNK = NormalType False "bool"
boolTypeK = NormalType True "bool"

expressions = [intConst 20 -- ^ Check constant integer
              ,(ExpBinOp Less (intConst 10) (ExpIdent $ Ident "a")) -- ^ Check binary expression
              ,(ExpFunCall $ FunCall (Ident "fun1")  -- ^ Check Function call expression
                [intConst 10, ExpBinOp Add (intConst 20) (ExpIdent $ Ident "a")]) 
              ,strConst "hi" -- ^ Check string literal
              ,(ExpUnaryOp BNot (ExpIdent $ Ident "b")) -- ^ Check unary expression
              ] 


expectedTypes = map (NormalType False) ["int"
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

                     -- Test full reduction to constant
injectExpressions = [(ExpBinOp Mul (ExpIdent (Ident "a")) (ExpLit (Number (Left 4))))
                     -- Test partial reduction
                    ,(ExpBinOp Less (ExpBinOp Add (ExpIdent $ Ident "a") (ExpIdent $ Ident "test"))  (
                                    (ExpBinOp Mul (intConst 5) (intConst 4))))
                    ]

expectedAfterInject = [intConst 24
                      ,ExpBinOp Less (ExpBinOp Add (intConst 6) (ExpIdent $ Ident "test"))
                                     (intConst 20) 
                      ]

-- Check that method calls cast the return type implicitly at compiler time
-- into a kernel type

programTempl stmts = Program [Func (intTypeNK) (Ident "test") [] $ BlockStmt stmts]

methodCalls = map programTempl 
    [[AssignStmt $ Assign intTypeNK (Ident "i") 
        (ExpMethodCall (MethodCall (Ident "obj") (Ident "m1") [intConst 5]))]]

expectedTCMethodCalls = map programTempl 
    [[AssignStmt $ Assign intTypeK (Ident "i") 
        (ExpMethodCall (MethodCall (Ident "obj") (Ident "m1") [intConst 5]))]]

-- Check that after assigning a method call result to a variable, that
-- that variable can't then be used in non-kernel ways 
invalidMethodUse = map programTempl 
                    [[(AssignStmt $ Assign intTypeNK (Ident "i") 
                        (ExpMethodCall (MethodCall (Ident "obj") (Ident "m1") []))),
                      (AssignStmt $ Assign intTypeNK (Ident "j") (ExpIdent $ Ident "i"))]

                    -- check kernel Boolean can't be used in if
                    ,[(AssignStmt $ Assign boolTypeNK (Ident "i") 
                        (ExpMethodCall (MethodCall (Ident "obj") (Ident "m1") []))),
                      (If (ExpIdent $ Ident "i") (BStmt $ BlockStmt []))]
                   ]

-- Programs which should pass type checking
validPrograms = [Program [TLAssign (Assign intTypeNK (Ident "i") (intConst 5))
                         ,TLAssign (Assign intTypeNK (Ident "j") (ExpIdent (Ident "i")))
                         ]
                ]


vars = M.fromList $ map (\(a,b) -> (Ident a, NormalType False b)) 
              [("a", "int")
              ,("b", "bool")
              ,("c", "double")
              ,("test", "int")
              ]
ftable = M.fromList [(Ident "fun1", (NormalType False "int" , 
            map (NormalType False) ["int", "int"]))
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

validProgramTest :: Program -> Test
validProgramTest p = TestCase (
    case result  of
        Left err -> assertFailure err
        Right _ -> unless (isRight' result) $ 
            assertFailure "This should never happen")
 where result = runTypeChecker p

-- Given a Program and an Expected reduced Program,
-- type check and reduce the given program and assert the
-- type checking is correct and the reduced program
-- matches the expected reduced program
typeCheckAndReduceTest :: Program -> Program -> Test
typeCheckAndReduceTest inP expectedOutP = TestCase (
    case result of
        Left err -> assertFailure err
        Right outP -> assertEqual "" outP expectedOutP)
  where result = runTypeChecker inP


-- Given a Program which should fail type checking
-- assert that it does actually fail when type checking.
invalidProgramTest :: Program -> Test
invalidProgramTest p = TestCase (
    unless (isLeft result) $ 
    assertFailure $ "Program should have caused a parse error" ++ show result)
 where isLeft = null . rights . return
       result = runTypeChecker p


validInject :: Expr -> Either String Expr -> Test
validInject e a = TestCase (
 case a of
    Left err -> assertFailure err
    Right p -> assertEqual "" (show p) (show e))

invalidTest :: Either String Type -> Test
invalidTest a = TestCase (
    unless (isLeft a) $ 
    assertFailure "Expected test to fail")
 where isLeft = null . rights . return

typeTests :: Test
typeTests = test $ (map (\(expected,expr) -> 
    validTest expected (getTypeExpr vars ftable expr)) (zip expectedTypes expressions)) ++
    (map (invalidTest . (getTypeExpr vars ftable) ) failExpressions) ++
    (map (\(expected, expr) -> 
        validInject expected (reduceExpr vars $ injectConstants ctable expr)) 
        (zip expectedAfterInject injectExpressions)
    ) ++ 
    (map validProgramTest validPrograms) ++
    (map invalidProgramTest invalidMethodUse) ++
    (map (\(expected, inProg) -> typeCheckAndReduceTest inProg expected) 
        (zip expectedTCMethodCalls methodCalls))

