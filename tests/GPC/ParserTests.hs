{- Unit Tests for parser -}
module GPC.ParserTests(parserTests) where

import Data.Either
import qualified Test.Framework.Providers.API as TFA
import Test.Framework.Providers.HUnit
import Test.HUnit
import Control.Monad
import GPC.Tests
import GPC.Parser
import GPC.AST


constructObjsCheck :: [(Program, Either String Program)]
constructObjsCheck = [singleObj, indexedObj]
  where 
    singleObj = 
        (Program 
            [TLConstructObjs $ 
                ConstructObjs (map Ident ["Test", "A", "A"]) (VarIdent $ Ident "a") []
            ]
        ,parseSource "a = Test::A::A();")
    
    indexedObj = 
        (Program 
            [TLConstructObjs $
                ConstructObjs (map Ident ["Test", "Blah", "B", "B"]) 
                    (VarArrayElem (Ident "b") (ExpLit $ Number (Left 10))) []
            ]
        ,parseSource "b[10] = Test::Blah::B::B();")

-- | Check declaring objects and arrays of objects works
objectsCheck :: [(Program , Either String Program)]
objectsCheck = [singleObj, largeNsSingleObj, arrObjs, arrObjsSp,
                spaceNameSpace1, spaceNameSpace2]
 where
   singleObj = (Program [TLObjs (Objects [Ident "Test", Ident "Obj"] (VarIdent $ Ident "obj"))],
               parseSource "Test::Obj obj;")

   largeNsSingleObj = (Program [TLObjs (Objects (map Ident ["Test1", "Test2", "Test3", "Test4", "Obj"]) 
                            (VarIdent $ Ident "obj"))],
                      parseSource "Test1::Test2::Test3::Test4::Obj obj;")

   arrObjs = (Program [TLObjs (Objects [Ident "Test", Ident "Obj"] 
                (VarArrayElem (Ident "obj") (ExpLit (Number (Left 10)))))],
               parseSource "Test::Obj obj[10];")
   
   -- Check space between array and name is skipped over
   arrObjsSp = (Program [TLObjs (Objects [Ident "Test", Ident "Obj"] 
                (VarArrayElem (Ident "obj") (ExpLit (Number (Left 10)))))],
               parseSource "Test::Obj obj [10];")

   -- Check spaces in namespace are skipped over
   spaceNameSpace1 = (Program [TLObjs (Objects (map Ident ["Test", "A", "B"]) (VarIdent $ Ident "d"))]
                     ,parseSource "Test   ::A::B d;")

   spaceNameSpace2 = (Program [TLObjs (Objects (map Ident ["Test", "A", "B", "C"]) 
                        (VarIdent $ Ident "g"))],
                      parseSource "Test::A::B  ::C g;")


invalidObjsCheck :: [Either String Program]
invalidObjsCheck = map parseSource [noNameSpace1, noNameSpace2, noVar] 
  where    
    noNameSpace1 = "Test a;"
    noNameSpace2 = "Stuff b[4];"
    noVar = "Test::A::B ;"


-- | Return expected and actual programs
-- | These are expected to pass and to be equal
assignCheck :: [(Program, Either String Program)]
assignCheck = [asInt, asChr, asBool, asDouble, asStr, asId]
 where
    -- |Check integer literal assignment
    asInt = (Program [TLAssign (Assign (NormalType False "int") (Ident "x") (ExpLit (Number (Left 20))))], 
            parseSource "int x = 20;")
    -- |Check char literal assignment
    asChr = (Program [TLAssign (Assign (NormalType False "char") (Ident "y") (ExpLit (Ch 'c')))], 
            parseSource "char y = 'c';")
    -- |Check bool literal assignment 
    asBool = (Program [TLAssign (Assign (NormalType False "bool") (Ident "b") (ExpLit (Bl False)))], 
            parseSource "bool b = false;")
    -- |Check float literal assignment
    asDouble = (Program [TLAssign (Assign (NormalType False "double") (Ident "d") (ExpLit (Number (Right 20.4))))], 
            parseSource "double d = 20.4;")
    -- |Check string literal assignment
    asStr = (Program [TLAssign (Assign (NormalType False "string") (Ident "s") (ExpLit (Str "hi")))], 
            parseSource "string s = \"hi\";")
    -- |Check identifier assignment
    asId = (Program [TLAssign (Assign (NormalType False "int") (Ident "i") (ExpIdent (Ident "x")))], 
            parseSource "int i =  x;")

-- | List of Programs which should fail assignment by the parser
assignFailCheck :: [Either String Program]
assignFailCheck = [noSemi, noAssign]
 where
    -- | No semicolon at end of statement
   noSemi = parseSource "int x ="
    -- | No equals, variables are single assignment
   noAssign = parseSource "float y;"
       
-- | Check binary operators are individually parsed
binOpCheck :: [(Program, Either String Program)]
binOpCheck = [ asMul, asEq, asShift, asPrece, asPrece2
             , asPrece3, asPrece4, asPrece5, asPrece6
             , asPrece7, asPrece8, parensCheck]
 where
    -- Check multiplication assignment of 2 identities
    asMul = (Program [TLAssign (Assign (NormalType False "int") (Ident "i") 
            (ExpBinOp Mul (ExpIdent (Ident "x")) (ExpIdent (Ident "y"))))] 
            ,parseSource "/* Check comment ignored */ int i = x * y;")
    -- Check equality assignment of a literal and identity
    asEq = (Program [TLAssign (Assign (NormalType False "bool") (Ident "b")
           (ExpBinOp Equals (ExpLit (Bl True)) (ExpIdent (Ident "b"))))]
           ,parseSource "bool b = true == b;")
    -- Check shift assignment of 3 values 
    asShift = (Program [TLAssign (Assign (NormalType False "int") (Ident "i")
            (ExpBinOp ShiftL ( ExpBinOp ShiftL (ExpLit (Number (Left 4))) (ExpLit (Number (Left 3))))
                   (ExpLit (Number (Left 2)))))]
            ,parseSource "int i = 4 << 3 << 2;")
    -- Check operator precedence works
    asPrece = (Program [TLAssign (Assign (NormalType False "int") (Ident "j")
              (ExpBinOp Add (ExpIdent (Ident "a")) (ExpBinOp Mul (
                ExpIdent (Ident "b")) (ExpIdent (Ident "c")))))]
              ,parseSource "int j = a + b * c;") 

    asPrece2 = (Program [TLAssign (Assign (NormalType False "int") (Ident "k")
               (ExpBinOp Add (ExpBinOp Mul 
               (ExpIdent (Ident "a")) (ExpIdent (Ident "b"))) (ExpIdent (Ident "c"))))]
               ,parseSource "int k = a * b + c;")

    asPrece3 = (Program [TLAssign (Assign (NormalType False "bool") (Ident "k")
               (ExpBinOp Equals (ExpBinOp Div 
               (ExpIdent (Ident "a")) (ExpIdent (Ident "b"))) (ExpIdent (Ident "c"))))]
               ,parseSource "bool k = a / b == c;")

    asPrece4 = (Program [TLAssign (Assign (NormalType False "int") (Ident "k")
               (ExpBinOp ShiftL (ExpBinOp Mod 
               (ExpIdent (Ident "a")) (ExpIdent (Ident "b"))) (ExpIdent (Ident "c"))))]
               ,parseSource "int k = a % b << c;")

    asPrece5 = (Program [TLAssign (Assign (NormalType False "bool") (Ident "k")
               (ExpBinOp Less (ExpBinOp ShiftR
               (ExpIdent (Ident "a")) (ExpIdent (Ident "b"))) (ExpIdent (Ident "c"))))]
               ,parseSource "bool k = a >> b < c;")

    asPrece6 = (Program [TLAssign (Assign (NormalType False "int") (Ident "k")
               (ExpBinOp BOr 
                 (ExpBinOp BXor 
                    (ExpBinOp BAnd (ExpIdent (Ident "a")) (ExpIdent (Ident "b"))) 
                    (ExpIdent (Ident "c"))
                 ) 
                 (ExpIdent (Ident "d"))
               ))]
               ,parseSource "int k = a & b ^ c | d;")
   
    asPrece7 = (Program [TLAssign (Assign (NormalType False "bool") (Ident "k")
               (ExpBinOp Or 
                 (ExpBinOp And 
                     (ExpBinOp LessEq  (ExpIdent (Ident "a")) (ExpIdent (Ident "b")))
                     (ExpBinOp Greater (ExpIdent (Ident "c")) (ExpIdent (Ident "d")))
                 )
                 (ExpBinOp GreaterEq (ExpIdent (Ident "e")) (ExpIdent (Ident "f")))
               )
               )]
               ,parseSource "bool k = a <= b && c > d || e >= f;")
    
    asPrece8 = (Program [TLAssign (Assign (NormalType False "bool") (Ident "k")
               (ExpBinOp NEquals (ExpBinOp ShiftR
               (ExpIdent (Ident "a")) (ExpIdent (Ident "b"))) (ExpIdent (Ident "c"))))]
               ,parseSource "bool k = a >> b != c;")

    -- Check precedence with parens
    parensCheck = (Program [TLAssign (Assign (NormalType False "int")  (Ident "l")
                  (ExpBinOp Mul (ExpIdent (Ident "a")) (ExpBinOp Add 
                    (ExpIdent (Ident "b")) (ExpIdent (Ident "c")))))]
                  ,parseSource "int l = a * (  b +  c);") 
    
       
-- | Check unary operators are individually parsed
unOpCheck :: [(Program, Either String Program)]
unOpCheck = [asNot, asPrece, asPrece2, asParens]
 where
    -- Check assignment of not identity
    asNot = (Program [TLAssign (Assign (NormalType False "bool") (Ident "i") 
            (ExpUnaryOp Not (ExpIdent (Ident "x"))))] 
            ,parseSource "bool i = !x;")
    -- Check precedence with binary operators   
    asPrece = (Program [TLAssign (Assign (NormalType False "int") (Ident "j")
             (ExpBinOp Add (ExpIdent (Ident "a")) (ExpUnaryOp BNot (ExpIdent (Ident "b")))))]
             ,parseSource "int j = a + ~b;")    
    -- Check precedence with binary operators   
    asPrece2 = (Program [TLAssign (Assign (NormalType False "int") (Ident "j")
             (ExpBinOp Add (ExpUnaryOp BNot (ExpIdent (Ident "a"))) (ExpIdent (Ident "b"))))]
             ,parseSource "int j = ~ a + b;")
    -- Check precedence with parenthesis
    asParens = (Program [TLAssign (Assign (NormalType False "int")  (Ident "k")
               (ExpUnaryOp Neg (ExpBinOp Sub (ExpIdent (Ident "a")) (ExpIdent (Ident "b")))))]
               ,parseSource "int k = -(a - b);")


-- | Check function calls made within functions are correctly parsed
funCallCheck :: [(Program, Either String Program)]
funCallCheck = [noArgs, singleArgs, multiArgs, multiComplexArgs, standAlone]
 where
    -- Check function with no arguments
    noArgs = (Program [fun $ [AssignStmt $ (Assign (NormalType False "int") (Ident "i")
             (ExpFunCall $ FunCall (Ident "test") []))]]
             ,parseSource $ funStr ++ "int i = test();" ++ "}")
    -- Check function with one argument
    singleArgs = (Program [fun $ [AssignStmt $ (Assign (NormalType False "test") (Ident "j")
                 (ExpFunCall $ FunCall (Ident "func") [ExpIdent (Ident "a")]))]]
                 ,parseSource $ funStr ++ "test j = func(a);" ++ "}")
    -- Check function with multiple arguments
    multiArgs = (Program [fun $ [AssignStmt $ (Assign (NormalType False "blarg") (Ident "m")
                (ExpFunCall $ FunCall (Ident "destroyAllHumans") [ExpIdent (Ident "a"), ExpIdent (Ident "b")]))]]
                ,parseSource$ funStr ++ "blarg m = destroyAllHumans(a, b);" ++ "}")
    -- Check function with multiple arguments with expressions
    multiComplexArgs = (Program [ fun $ [AssignStmt $ (Assign (NormalType False "int") (Ident "a")
                      (ExpFunCall $ FunCall (Ident "call") [ExpBinOp Mul (ExpIdent (Ident "a")) (ExpIdent (Ident "b")),
                      ExpUnaryOp Neg (ExpIdent (Ident "c"))]))]]
                      ,parseSource $ funStr ++ "int a = call(a * b, -c);" ++ "}")

    -- Check function call statement
    standAlone = (Program [fun [FunCallStmt $ FunCall (Ident "call") []]]
                  ,parseSource $ funStr ++ "call();" ++ "}")

    fun xs = Func (NormalType False "tes") (Ident "test") [] (BlockStmt xs)
    funStr = "tes test() {"


-- | Check method calls made within functions are correctly parsed
methodCallCheck :: [(Program, Either String Program)]
methodCallCheck = [noArgs, singleArgs, multiArgs, multiComplexArgs, standAlone, standAloneIndexed]
 where
    -- Check method with no arguments
    noArgs = (Program [fun $ [AssignStmt $ (Assign (NormalType False "int") (Ident "i")
             (ExpMethodCall $ MethodCall (VarIdent $ Ident "a") (Ident "test") []))]]
             ,parseSource $ funStr ++ "int i = a.test();" ++ "}")
    -- Check method with one argument
    singleArgs = (Program [fun $ [AssignStmt $ (Assign (NormalType False "test") (Ident "j")
                 (ExpMethodCall $ MethodCall (VarIdent $ Ident "a") (Ident "func") 
                    [ExpIdent (Ident "a")]))]]
                 ,parseSource $ funStr ++ "test j = a.func(a);" ++ "}")
    -- Check method with multiple arguments
    multiArgs = (Program [fun $ [AssignStmt $ (Assign (NormalType False "blarg") (Ident "m")
                (ExpMethodCall $ MethodCall (VarIdent $ Ident "a") (Ident "destroyAllHumans") 
                    [ExpIdent (Ident "a"), ExpIdent (Ident "b")]))]]
                ,parseSource$ funStr ++ "blarg m = a.destroyAllHumans(a, b);" ++ "}")
    -- Check method with multiple arguments with expressions
    multiComplexArgs = (Program [ fun $ [AssignStmt $ (Assign (NormalType False "int") (Ident "a")
                      (ExpMethodCall $ MethodCall (VarIdent $ Ident "a") (Ident "call") 
                        [ExpBinOp Mul (ExpIdent (Ident "a")) (ExpIdent (Ident "b"))
                        ,ExpUnaryOp Neg (ExpIdent (Ident "c"))]))
                        ]]
                      ,parseSource $ funStr ++ "int a = a.call(a * b, -c);" ++ "}")

    -- Check method call statement
    standAlone = (Program [fun [MethodStmt $ MethodCall (VarIdent $ Ident "a") (Ident "call") []]]
                  ,parseSource $ funStr ++ "a.call();" ++ "}")
    --
    standAloneIndexed = 
        (Program [fun [MethodStmt $ MethodCall (VarArrayElem (Ident "a") (ExpLit (Number $ Left 2))) 
            (Ident "call") []]]
        ,parseSource $ funStr ++ "a[2].call();" ++ "}")

    fun xs = Func (NormalType False "tes") (Ident "test") [] (BlockStmt xs)
    funStr = "tes test() {"

-- | Check sequential/parallel blocks are correctly parsed
seqParBlockCheck :: [(Program, Either String Program)]
seqParBlockCheck = [seqB, parB, seqMultiB]
 where
    seqB = (Program [fun [(Seq $ BlockStmt [AssignStmt $ Assign 
            (NormalType False "int") (Ident "i") (ExpIdent (Ident "x"))])]],
           parseSource  $ funStr ++ "seq {int i = x;}" ++ "}")
    
    parB = (Program [fun [(BStmt $ BlockStmt [AssignStmt $ Assign 
            (NormalType False "int") (Ident "i") (ExpIdent (Ident "x"))])]],
           parseSource $ funStr  ++ "par {int i = x;}" ++ "}")

    seqMultiB = (Program [fun [ (Seq $ BlockStmt [AssignStmt $ Assign 
            (NormalType False "int") (Ident "i") (ExpIdent (Ident "x")),
           AssignStmt $ Assign (NormalType False "int") (Ident "j") (ExpIdent (Ident "y"))])]],
           parseSource $ funStr ++ "seq {int i = x; int j = y;}" ++ "}")
    fun xs = Func (NormalType False "tes") (Ident "test") [] (BlockStmt xs)
    funStr = "tes test() {"

-- | Check If-Else statements are correctly parsed
ifElseCheck :: [(Program, Either String Program)]
ifElseCheck = [ifCheck, elseCheck]
 where
    -- Check If by itself
    ifCheck = (Program [fun [ 
              (If (ExpUnaryOp Not (ExpIdent (Ident "x"))) 
                (BStmt $ BlockStmt [Return $ ExpIdent (Ident "y")])
              )]],
              parseSource $ funStr ++ "if (!x) {return y;}" ++ "}") 
    -- Check If with Else
    elseCheck = (Program [fun [
                (IfElse (ExpUnaryOp Not (ExpIdent (Ident "z")))
                    (Return $ ExpIdent (Ident "y"))
                    (Return $ ExpIdent (Ident "a"))
                )]],
                parseSource $ funStr ++ "if (!z) return y; else return a;" ++ "}")
    fun xs = Func (NormalType False "tes") (Ident "test") [] (BlockStmt xs)
    funStr = "tes test() {"

-- | Check For loop statements are correctly parsed
forLoopCheck :: [(Program, Either String Program)]
forLoopCheck = [forCheck]
  where
    forCheck = (Program [fun [ 
              (ForLoop (Ident "a") (ExpUnaryOp Neg (ExpLit $ Number $ Left 17)) 
                (ExpIdent $ Ident "b") (ExpLit $ Number $ Left 1) (BlockStmt []))]] 
              , parseSource $ funStr ++ "for (int a = -17; b; 1) {}" ++ "}") 

    fun xs = Func (NormalType False "tes") (Ident "test") [] (BlockStmt xs)
    funStr = "tes test() {"
    

-- | Check Pointer
pointersCheck :: [(Program, Either String Program)]
pointersCheck = [singlePtr, mulPtr]
  where
     singlePtr = (Program [Func intType (Ident "test") [(PointerType intType, a)] 
                            $ BlockStmt [AssignStmt 
                                $ Assign (PointerType intType) b (ExpIdent a)]]
                 ,parseSource "int test(int *a) {int *b = a;}") 


     mulPtr = (Program [Func intType (Ident "test") [(triplePtr intType, a)] 
                            $ BlockStmt [AssignStmt 
                                $ Assign (triplePtr intType) b (ExpIdent a)]]
                 ,parseSource "int test(int ***a) {int ***b = a;}") 

     intType = NormalType False "int"
     triplePtr = PointerType . PointerType . PointerType
     a = Ident "a"
     b = Ident "b"

validTest :: String -> (Program, Either String Program) -> TFA.Test
validTest label (e, a) = testCase label (
 case a of
    Left err -> assertFailure err
    Right p -> assertEqual "" e p)

validTests :: String -> [(Program, Either String Program)] -> [TFA.Test]
validTests s ps = map (uncurry validTest) $ zip labels ps
    where labels = makeLabels s ps

validObjectsTests :: [TFA.Test]
validObjectsTests = validTests "validObjectDecl" objectsCheck

validObjectConsTests :: [TFA.Test]
validObjectConsTests = validTests "validObjectCons" constructObjsCheck


-- | Test valid assignments 
validAssignTests :: [TFA.Test]
validAssignTests = validTests "validAssignTest" assignCheck

-- | Test valid assignments with binary operators
validOpTests :: [TFA.Test]
validOpTests = validTests "validOpTest" binOpCheck

-- | Test valid assignments with unary operators
validUnOpTests :: [TFA.Test]
validUnOpTests = validTests "validUnOpTest" unOpCheck

-- | Test valid function call expressions/statements
validFunCallTests :: [TFA.Test]
validFunCallTests = validTests "validFunCallTest" funCallCheck

-- | Test valid method call expressions/statements
validMethodCallTests :: [TFA.Test]
validMethodCallTests = validTests "validMethodCallTest" methodCallCheck

-- | Test valid sequential/parallel blocks
validSeqParTests :: [TFA.Test]
validSeqParTests = validTests "seqParTest" seqParBlockCheck

-- | Test valid if/else statements
validIfElseTests :: [TFA.Test]
validIfElseTests = validTests "ifElseTest" ifElseCheck

-- | Test valid for loops
validForLoopTests :: [TFA.Test]
validForLoopTests = validTests "forLoopTest" forLoopCheck

-- | Test Pointer Parsing
validPointerTests :: [TFA.Test]
validPointerTests = validTests "pointerTest" pointersCheck

-- | Test invalid statement
invalidTest :: String -> Either String Program -> TFA.Test
invalidTest label a = testCase label (
    unless (isLeft a) $ 
    assertFailure $ "Program should have caused a parse error" ++ show a)
 where isLeft = null . rights . return

invalidAssignTests :: [TFA.Test]
invalidAssignTests = map (uncurry invalidTest) $ zip labels assignFailCheck
 where labels = makeLabels "invalidAssignTest" assignFailCheck

invalidObjectsTests :: [TFA.Test]
invalidObjectsTests = map (uncurry invalidTest) $ zip labels invalidObjsCheck
 where labels = makeLabels "invalidObjDeclTest" invalidObjsCheck


parserTests = TFA.testGroup "Parser Tests" $ concat
    [validAssignTests, invalidAssignTests
    ,validOpTests, validUnOpTests
    ,validFunCallTests, validSeqParTests
    ,validIfElseTests, validForLoopTests, validObjectsTests
    ,validObjectConsTests, validMethodCallTests
    ,invalidObjectsTests, validPointerTests
    ]
