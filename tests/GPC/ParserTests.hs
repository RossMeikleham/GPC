{- Unit Tests for parser -}
module GPC.ParserTests(parserTests) where

import Data.Either
import Control.Monad
import Test.HUnit
import GPC.Parser
import GPC.AST
import GPC.Tests

-- | Return expected and actual programs
-- | These are expected to pass and to be equal
assignCheck :: [(Program, Either String Program)]
assignCheck = [asInt, asChr, asBool, asDouble, asStr, asId]
 where
    -- |Check integer literal assignment
    asInt = (Program [TLAssign (Assign (Type "int") (Ident "x") (ExpLit (Num (Left 20))))], 
            parseSource "int x = 20;")
    -- |Check char literal assignment
    asChr = (Program [TLAssign (Assign (Type "char") (Ident "y") (ExpLit (Ch 'c')))], 
            parseSource "char y = 'c';")
    -- |Check bool literal assignment 
    asBool = (Program [TLAssign (Assign (Type "bool") (Ident "b") (ExpLit (Bl False)))], 
            parseSource "bool b = false;")
    -- |Check float literal assignment
    asDouble = (Program [TLAssign (Assign (Type "double") (Ident "d") (ExpLit (Num (Right 20.4))))], 
            parseSource "double d = 20.4;")
    -- |Check string literal assignment
    asStr = (Program [TLAssign (Assign (Type "string") (Ident "s") (ExpLit (Str "hi")))], 
            parseSource "string s = \"hi\";")
    -- |Check identifier assignment
    asId = (Program [TLAssign (Assign (Type "int") (Ident "i") (ExpIdent (Ident "x")))], 
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
    asMul = (Program [TLAssign (Assign (Type "int") (Ident "i") 
            (ExpBinOp Mul (ExpIdent (Ident "x")) (ExpIdent (Ident "y"))))] 
            ,parseSource "int i = x * y;")
    -- Check equality assignment of a literal and identity
    asEq = (Program [TLAssign (Assign (Type "bool") (Ident "b")
           (ExpBinOp Equals (ExpLit (Bl True)) (ExpIdent (Ident "b"))))]
           ,parseSource "bool b = true == b;")
    -- Check shift assignment of 3 values 
    asShift = (Program [TLAssign (Assign (Type "int") (Ident "i")
            (ExpBinOp ShiftL ( ExpBinOp ShiftL (ExpLit (Num (Left 4))) (ExpLit (Num (Left 3))))
                   (ExpLit (Num (Left 2)))))]
            ,parseSource "int i = 4 << 3 << 2;")
    -- Check operator precedence works
    asPrece = (Program [TLAssign (Assign (Type "int") (Ident "j")
              (ExpBinOp Add (ExpIdent (Ident "a")) (ExpBinOp Mul (
                ExpIdent (Ident "b")) (ExpIdent (Ident "c")))))]
              ,parseSource "int j = a + b * c;") 

    asPrece2 = (Program [TLAssign (Assign (Type "int") (Ident "k")
               (ExpBinOp Add (ExpBinOp Mul 
               (ExpIdent (Ident "a")) (ExpIdent (Ident "b"))) (ExpIdent (Ident "c"))))]
               ,parseSource "int k = a * b + c;")
    -- Check precedence with parens
    parensCheck = (Program [TLAssign (Assign (Type "int")  (Ident "l")
                  (ExpBinOp Mul (ExpIdent (Ident "a")) (ExpBinOp Add 
                    (ExpIdent (Ident "b")) (ExpIdent (Ident "c")))))]
                  ,parseSource "int l = a * (  b +  c);") 
    
       
-- | Check unary operators are individually parsed
unOpCheck :: [(Program, Either String Program)]
unOpCheck = [asNot, asPrece, asPrece2, asParens]
 where
    -- Check assignment of not identity
    asNot = (Program [TLAssign (Assign (Type "bool") (Ident "i") 
            (ExpUnaryOp Not (ExpIdent (Ident "x"))))] 
            ,parseSource "bool i = !x;")
    -- Check precedence with binary operators   
    asPrece = (Program [TLAssign (Assign (Type "int") (Ident "j")
             (ExpBinOp Add (ExpIdent (Ident "a")) (ExpUnaryOp BNot (ExpIdent (Ident "b")))))]
             ,parseSource "int j = a + ~b;")    
    -- Check precedence with binary operators   
    asPrece2 = (Program [TLAssign (Assign (Type "int") (Ident "j")
             (ExpBinOp Add (ExpUnaryOp BNot (ExpIdent (Ident "a"))) (ExpIdent (Ident "b"))))]
             ,parseSource "int j = ~ a + b;")
    -- Check precedence with parenthesis
    asParens = (Program [TLAssign (Assign (Type "int")  (Ident "k")
               (ExpUnaryOp Neg (ExpBinOp Sub (ExpIdent (Ident "a")) (ExpIdent (Ident "b")))))]
               ,parseSource "int k = -(a - b);")


-- | Check function calls made within functions are correctly parsed
funCallCheck :: [(Program, Either String Program)]
funCallCheck = [noArgs, singleArgs, multiArgs, multiComplexArgs]
 where
    -- Check function with no arguments
    noArgs = (Program [fun $ [AssignStmt $ (Assign (Type "int") (Ident "i")
             (ExpFunCall $ FunCall "test" []))]]
             ,parseSource $ funStr ++ "int i = test();" ++ "}")
    -- Check function with one argument
    singleArgs = (Program [fun $ [AssignStmt $ (Assign (Type "test") (Ident "j")
                 (ExpFunCall $ FunCall "func" [ExpIdent (Ident "a")]))]]
                 ,parseSource $ funStr ++ "test j = func(a);" ++ "}")
    -- Check function with multiple arguments
    multiArgs = (Program [fun $ [AssignStmt $ (Assign (Type "blarg") (Ident "m")
                (ExpFunCall $ FunCall"destroyAllHumans" [ExpIdent (Ident "a"), ExpIdent (Ident "b")]))]]
                ,parseSource$ funStr ++ "blarg m = destroyAllHumans(a, b);" ++ "}")
    -- Check function with multiple arguments with expressions
    multiComplexArgs = (Program [ fun $ [AssignStmt $ (Assign (Type "int") (Ident "a")
                      (ExpFunCall $ FunCall "call" [ExpBinOp Mul (ExpIdent (Ident "a")) (ExpIdent (Ident "b")),
                      ExpUnaryOp Neg (ExpIdent (Ident "c"))]))]]
                      ,parseSource $ funStr ++ "int a = call(a * b, -c);" ++ "}")
    fun xs = Func (Type "tes") (Ident "test") [] (BlockStmt xs)
    funStr = "tes test() {"

-- | Check sequential/parallel blocks are correctly parsed
seqParBlockCheck :: [(Program, Either String Program)]
seqParBlockCheck = [seqB, parB, seqMultiB]
 where
    seqB = (Program [fun [(Seq $ BlockStmt [AssignStmt $ Assign 
            (Type "int") (Ident "i") (ExpIdent (Ident "x"))])]],
           parseSource  $ funStr ++ "seq {int i = x;}" ++ "}")
    
    parB = (Program [fun [(BStmt $ BlockStmt [AssignStmt $ Assign 
            (Type "int") (Ident "i") (ExpIdent (Ident "x"))])]],
           parseSource $ funStr  ++ "par {int i = x;}" ++ "}")

    seqMultiB = (Program [fun [ (Seq $ BlockStmt [AssignStmt $ Assign 
            (Type "int") (Ident "i") (ExpIdent (Ident "x")),
           AssignStmt $ Assign (Type "int") (Ident "j") (ExpIdent (Ident "y"))])]],
           parseSource $ funStr ++ "seq {int i = x; int j = y;}" ++ "}")
    fun xs = Func (Type "tes") (Ident "test") [] (BlockStmt xs)
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
    fun xs = Func (Type "tes") (Ident "test") [] (BlockStmt xs)
    funStr = "tes test() {"

validTest :: Program -> Either String Program -> Test
validTest e a = TestCase (
 case a of
    Left err -> assertFailure err
    Right p -> assertEqual "" (show p) (show e))

validTests :: String -> [(Program, Either String Program)] -> Test
validTests s ps = makeLabels s tests
    where tests = map (uncurry validTest) ps

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

-- | Test invalid variable assignment statement
invalidAssignTest :: Either String Program -> Test
invalidAssignTest a = TestCase (
    unless (isLeft a) $ 
    assertFailure "Program should have caused a parse error")
 where isLeft = null . rights . return

invalidAssignTests :: Test
invalidAssignTests = makeLabels "invalidAssignTest" tests
 where tests = map invalidAssignTest assignFailCheck


parserTests :: Test
parserTests = TestList [validAssignTests, invalidAssignTests
                       ,validOpTests, validUnOpTests
                       ,validFunCallTests, validSeqParTests
                       ,validIfElseTests
                       ]
