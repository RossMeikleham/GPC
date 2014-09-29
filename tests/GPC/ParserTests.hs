{- Unit Tests for parser -}
module GPC.ParserTests(parserTests) where

import Data.Either
import Control.Monad
import Test.HUnit
import GPC.Parser
import GPC.AST

-- |Return expected and actual programs
-- |These are expected to pass and to be equal
assignCheck :: [(Program, Either String Program)]
assignCheck = [asInt, asChr, asBool, asDouble, asStr, asId]
 where
    -- |Check integer literal assignment
    asInt = (Program [TlStmt (Decl "int" "x" (Lit (Num (Left 20))))], parseSource "int x = 20;")
    -- |Check char literal assignment
    asChr = (Program [TlStmt (Decl "char" "y" (Lit (Ch 'c')))], parseSource "char y = 'c';")
    -- |Check bool literal assignment 
    asBool = (Program [TlStmt (Decl "bool" "b" (Lit (Bl False)))], parseSource "bool b = false;")
    -- |Check float literal assignment
    asDouble = (Program [TlStmt (Decl "double" "d" (Lit (Num (Right 20.4))))], parseSource "double d = 20.4;")
    -- |Check string literal assignment
    asStr = (Program [TlStmt (Decl "string" "s" (Lit (Str "hi")))], parseSource "string s = \"hi\";")
    -- |Check identifier assignment
    asId = (Program [TlStmt (Decl "int" "i" (Ident "x"))], parseSource "int i =  x;")


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
    asMul = (Program [TlStmt (Decl "int" "i" 
            (BinOp Mul (Ident "x") (Ident "y")))] 
            ,parseSource "int i = x * y;")
    -- Check equality assignment of a literal and identity
    asEq = (Program [TlStmt (Decl "bool" "b"
           (BinOp Equals (Lit (Bl True)) (Ident "b")))]
           ,parseSource "bool b = true == b;")
    -- Check shift assignment of 3 values 
    asShift = (Program [TlStmt (Decl "int" "i"
            (BinOp ShiftL (BinOp ShiftL (Lit (Num (Left 4))) (Lit (Num (Left 3))))
                   (Lit (Num (Left 2)))))]
            ,parseSource "int i = 4 << 3 << 2;")
    -- Check operator precedence works
    asPrece = (Program [TlStmt (Decl "int" "j"
              (BinOp Add (Ident "a") (BinOp Mul (Ident "b") (Ident "c"))))]
              ,parseSource "int j = a + b * c;") 

    asPrece2 = (Program [TlStmt (Decl "int" "k"
               (BinOp Add (BinOp Mul (Ident "a") (Ident "b")) (Ident "c")))]
               ,parseSource "int k = a * b + c;")
    -- Check precedence with parens
    parensCheck = (Program [TlStmt (Decl "int" "l"
                  (BinOp Mul (Ident "a") (BinOp Add (Ident "b") (Ident "c"))))]
                  ,parseSource "int l = a * (  b +  c);")
    
-- | Test valid statements
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

-- | Test invalid variable assignment statement
invalidAssignTest :: Either String Program -> Test
invalidAssignTest a = TestCase (
    unless (isLeft a) $ 
    assertFailure "Program should have caused a parse error")
 where isLeft = null . rights . return

invalidAssignTests :: Test
invalidAssignTests = makeLabels "invalidAssignTest" tests
 where tests = map invalidAssignTest assignFailCheck


-- | Make labels for test cases given a string for example given name
-- |assign, creates labels assign1 assign2 etc for tests
makeLabels :: String -> [Test] -> Test
makeLabels str tests = TestList $ zipWith TestLabel labels tests
 where labels = map(str ++) nums
       nums = map showInt [1..]
       showInt :: Int -> String
       showInt = show

parserTests :: Test
parserTests = TestList [validAssignTests, invalidAssignTests, validOpTests]
