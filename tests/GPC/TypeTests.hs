-- Unit Tests for the type checker

module GPC.TypeTests(typeTests) where

import Test.HUnit
import qualified Test.Framework.Providers.API as TFA
import Test.Framework.Providers.HUnit
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
--boolTypeK = NormalType True "bool"

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

programTempl stmts = Program 
    [TLObjs $ Objects (map Ident ["GPRM","Kernel","A"]) (VarIdent $ Ident "obj")    
    ,Func (intTypeNK) (Ident "test") [] $ BlockStmt stmts
    ]

methodCalls = map programTempl 
    [[AssignStmt $ Assign intTypeNK (Ident "i") 
        (ExpMethodCall (MethodCall (VarIdent $ Ident "obj") (Ident "m1") [intConst 5]))

     ,MethodStmt $ MethodCall (VarIdent $ Ident "obj") (Ident "m1") [intConst 32]
     ]
    ]

expectedTCMethodCalls = map programTempl 
    [[AssignStmt $ Assign intTypeK (Ident "i") 
        (ExpMethodCall (MethodCall (VarIdent $ Ident "obj") (Ident "m1") [intConst 5]))

     ,MethodStmt $ MethodCall (VarIdent $ Ident "obj") (Ident "m1") [intConst 32]
     ]
    ]

-- Check Pointers type checked correctly, and reduced
pointerProgramTempl stmts = Program [Func (intTypeNK) (Ident "test") 
                                [(PointerType intTypeNK, Ident "a")] $ BlockStmt stmts]
pointerAssigns = map pointerProgramTempl
    [[AssignStmt $ Assign (PointerType intTypeNK) (Ident "b")
        (ExpBinOp Add (ExpIdent $ Ident "a") 
            (ExpBinOp Add (intConst 4) (intConst 3)))
     ]
    ]

expectedTCPointerAssigns = map pointerProgramTempl
    [[AssignStmt $ Assign (PointerType intTypeNK) (Ident "b")
        (ExpBinOp Add (ExpIdent $ Ident "a") 
            (ExpBinOp Add (intConst 4) (intConst 3)))
     ]
    ]


-- Check that after assigning a method call result to a variable, that
-- that variable can't then be used in non-kernel ways 
invalidMethodUse = map programTempl 
                    [[(AssignStmt $ Assign intTypeNK (Ident "i") 
                        (ExpMethodCall (MethodCall (VarIdent $ Ident "obj") (Ident "m1") []))),
                      (AssignStmt $ Assign intTypeNK (Ident "j") (ExpIdent $ Ident "i"))]

                    -- check kernel Boolean can't be used in if
                    ,[(AssignStmt $ Assign boolTypeNK (Ident "i") 
                        (ExpMethodCall (MethodCall (VarIdent $ Ident "obj") (Ident "m1") []))),
                      (If (ExpIdent $ Ident "i") (BStmt $ BlockStmt []))]
                   ]


-- Check multiple variables can't be declared in the same scope
multipleDefInScope = 
    -- Check identifiers on top level scope not duplicated
    [Program [TLAssign (Assign intTypeNK (Ident "i") (intConst 5))
             ,TLAssign (Assign intTypeNK (Ident "i") (intConst 3))
             ]
     -- Check function arguments not duplicated    
     ,Program [Func (intTypeNK) (Ident "mulArgsTest") 
                [(intTypeNK, Ident "a"), (boolTypeNK, Ident "a")] $ BlockStmt []
              ]                
     -- Check function argument, not also present in main
     -- function block
     ,Program [Func (intTypeNK) (Ident "argsFunDups") [(boolTypeNK, Ident "b")] $ BlockStmt
                [AssignStmt $ Assign intTypeNK (Ident "b") (intConst 42)]
              ]
     ]                  

-- Check constructors which shouldn't compile
checkConstructors =
  map objTempl  
    -- Check assigning an object not declared doesn't work
    [[TLConstructObjs $ ConstructObjs 
        (map Ident ["GPRM", "Kernel", "A", "A"]) 
        (VarIdent $ Ident "e") []
     ]   
   -- Check constructor syntax is correct
   ,[TLConstructObjs $ ConstructObjs 
        (map Ident ["GPRM", "Kernel", "A", "C"]) 
        (VarIdent $ Ident "o") []
    ]
    -- Check constructing array element for something not in an array
   ,[TLConstructObjs $ ConstructObjs 
        (map Ident ["GPRM", "Kernel", "A", "A"]) 
        (VarArrayElem (Ident "o") (intConst 0)) []]

   -- Check assigning an array element of an object not declared doesn't
   -- work
   ,[TLConstructObjs $ ConstructObjs 
        (map Ident ["GPRM", "Kernel", "A", "D"]) 
        (VarArrayElem (Ident "e") (intConst 0)) []
    ]
   -- Check constructor syntax for array elements are correct
   ,[TLConstructObjs $ ConstructObjs 
        (map Ident ["GPRM", "Kernel", "A", "C"]) 
        (VarArrayElem (Ident "t") (intConst 0)) []
    ]
    -- Check constructing object for an array
   ,[TLConstructObjs $ ConstructObjs 
        (map Ident ["GPRM", "Kernel", "A", "D"]) 
        (VarIdent $ Ident "t") []
    ]
   -- Check out of bounds array access
   ,[TLConstructObjs $ ConstructObjs 
        (map Ident ["GPRM", "Kernel", "A", "D"]) 
        (VarArrayElem (Ident "t") (intConst 2)) []
    ]
   -- Check out of bounds array access (negative element)
   ,[TLConstructObjs $ ConstructObjs 
        (map Ident ["GPRM", "Kernel", "A", "D"]) 
        (VarArrayElem (Ident "t") (intConst (-1))) []
    ]
   ]
    where 
        objTempl :: [TopLevel] -> Program
        objTempl stmts = Program (
            [TLObjs $ Objects (map Ident ["GPRM", "Kernel", "A"]) 
                              (VarIdent $ Ident "o")
                                           
            ,TLObjs $ Objects (map Ident ["GPRM", "Kernel", "D"]) 
                              (VarArrayElem (Ident "t") (intConst 2))]
            ++ stmts)
                
-- Programs which should pass type checking
validPrograms = [Program [TLAssign (Assign intTypeNK (Ident "i") (intConst 5))
                         ,TLAssign (Assign intTypeNK (Ident "j") (ExpIdent (Ident "i")))
                         ]
                -- Check single object declarations and construction
                ,Program [TLObjs $ Objects (map Ident ["GPRM", "Kernel", "A"]) 
                                           (VarIdent $ Ident "o"),
                          TLConstructObjs $ ConstructObjs 
                            (map Ident ["GPRM", "Kernel", "A", "A"]) 
                            (VarIdent $ Ident "o")
                            [intConst 72] 
                         ]
                -- Check array object declarations and construction   
                ,Program [TLObjs $ Objects (map Ident ["GPRM", "Kernel", "A"]) 
                                           (VarArrayElem (Ident "o") (intConst 2)),
                          TLConstructObjs $ ConstructObjs 
                            (map Ident ["GPRM", "Kernel", "A", "A"]) 
                            (VarArrayElem (Ident "o") (intConst 1))
                            [intConst 72],
                            
                          TLConstructObjs $ ConstructObjs 
                            (map Ident ["GPRM", "Kernel", "A", "A"]) 
                            (VarArrayElem (Ident "o") (intConst 0))
                            [intConst 32] 
                           
                         ]
                ]


vars = M.fromList $ map (\(a,b) -> (Ident a, NormalType False b)) 
              [("a", "int")
              ,("b", "int")
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

 
validTest :: Type -> Either String Type -> TFA.Test
validTest e a = testCase "type check passed test" (
 case a of
    Left err -> assertFailure err
    Right p -> assertEqual "" (show p) (show e))

validProgramTest :: Program -> TFA.Test
validProgramTest p = testCase "Checking full programs" (
    case result  of
        Left err -> assertFailure err
        Right _ -> unless (isRight' result) $ 
            assertFailure "This should never happen")
 where result = runTypeChecker p

-- Given a Program and an Expected reduced Program,
-- type check and reduce the given program and assert the
-- type checking is correct and the reduced program
-- matches the expected reduced program
typeCheckAndReduceTest :: Program -> Program -> TFA.Test
typeCheckAndReduceTest inP expectedOutP = testCase "Checking type/scope and reduction" (
    case result of
        Left err -> assertFailure err
        Right outP -> assertEqual "" outP expectedOutP)
  where result = runTypeChecker inP


-- Given a Program which should fail type checking
-- assert that it does actually fail when type checking.
invalidProgramTest :: Program -> TFA.Test
invalidProgramTest p = testCase "error catching test for full Program" (
    unless (isLeft result) $ 
    assertFailure $ "Program should have contained type/scope error" ++ show result)
 where isLeft = null . rights . return
       result = runTypeChecker p


validInject :: Expr -> Either String Expr -> TFA.Test
validInject e a = testCase "injectingConstants" (
 case a of
    Left err -> assertFailure err
    Right p -> assertEqual "" (show p) (show e))

invalidTest :: Either String Type -> TFA.Test
invalidTest a = testCase "Error catching test" (
    unless (isLeft a) $ 
    assertFailure "Expected test to fail")
 where isLeft = null . rights . return

typeTests :: TFA.Test
typeTests = TFA.testGroup "Type/Scope Tests" $ (map (\(expected,expr) -> 
    validTest expected (getTypeExpr vars ftable expr)) (zip expectedTypes expressions)) ++
    (map (invalidTest . (getTypeExpr vars ftable) ) failExpressions) ++
    (map validProgramTest validPrograms) ++
    (map invalidProgramTest $ invalidMethodUse ++ multipleDefInScope ++ 
                              checkConstructors ) ++
    (map (\(expected, inProg) -> typeCheckAndReduceTest inProg expected) 
        (zip expectedTCMethodCalls methodCalls)) ++
    (map (\(expected, inProg) -> typeCheckAndReduceTest inProg expected) 
        (zip expectedTCPointerAssigns pointerAssigns))

    

