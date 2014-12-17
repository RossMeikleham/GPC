{- Unit Tests for parser -}
module GPC.ParserTests(parserTests) where

import Data.Either
import Control.Monad
import Test.HUnit
import GPC.Parser
import GPC.AST
import GPC.Tests

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
binOpCheck = [asMul, asEq, asShift, asPrece, asPrece2, parensCheck]
 where
    -- Check multiplication assignment of 2 identities
    asMul = (Program [TLAssign (Assign (NormalType False "int") (Ident "i") 
            (ExpBinOp Mul (ExpIdent (Ident "x")) (ExpIdent (Ident "y"))))] 
            ,parseSource "int i = x * y;")
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
funCallCheck = [noArgs, singleArgs, multiArgs, multiComplexArgs]
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

validTest :: Program -> Either String Program -> Test
validTest e a = TestCase (
 case a of
    Left err -> assertFailure err
    Right p -> assertEqual "" (show p) (show e))

validTests :: String -> [(Program, Either String Program)] -> Test
validTests s ps = makeLabels s tests
    where tests = map (uncurry validTest) ps

validObjectsTests :: Test
validObjectsTests = validTests "validObjectDecl" objectsCheck


-- | Test valid assignments 
validAssignTests :: Test
validAssignTests = validTests "validAssignTest" assignCheck

-- | Test valid assignments with binary operators
validOpTests :: Test
validOpTests = validTests "validOpTest" binOpCheck

-- | Test valid assignments with unary operators
validUnOpTests :: Test
validUnOpTests = validTests "validUnOpTest" unOpCheck

-- | Test valid function call expressions
validFunCallTests :: Test
validFunCallTests = validTests "validFunCallTest" funCallCheck

-- | Test valid sequential/parallel blocks
validSeqParTests :: Test
validSeqParTests = validTests "seqParTest" seqParBlockCheck

-- | Test valid if/else statements
validIfElseTests :: Test
validIfElseTests = validTests "ifElseTest" ifElseCheck

-- | Test invalid statement
invalidTest :: Either String Program -> Test
invalidTest a = TestCase (
    unless (isLeft a) $ 
    assertFailure $ "Program should have caused a parse error" ++ show a)
 where isLeft = null . rights . return

invalidAssignTests :: Test
invalidAssignTests = makeLabels "invalidAssignTest" tests
 where tests = map invalidTest assignFailCheck

invalidObjectsTests :: Test
invalidObjectsTests = makeLabels "invalidObjDeclTest" tests
 where tests = map invalidTest invalidObjsCheck


parserTests :: Test
parserTests = TestList [validAssignTests, invalidAssignTests
                       ,validOpTests, validUnOpTests
                       ,validFunCallTests, validSeqParTests
                       ,validIfElseTests, validObjectsTests
                       ,invalidObjectsTests
                       ]
