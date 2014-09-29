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
     


-- | Test valid variable assignment statement
validAssignTest :: Program -> Either String Program -> Test
validAssignTest e a = TestCase (
 case a of
    Left err -> assertFailure err
    Right p -> assertEqual "" (show p) (show e))


validAssignTests :: Test
validAssignTests = makeLabels "validAssignTest" tests
 where tests = map (uncurry validAssignTest) assignCheck

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
parserTests = TestList [validAssignTests, invalidAssignTests]
