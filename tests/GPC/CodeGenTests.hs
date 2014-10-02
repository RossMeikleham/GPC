{- Test Suite for Code Generating -}

module GPC.CodeGenTests (codeGenTests) where

import Test.HUnit
--import GPC.CodeGen
import GPC.Tests

-- | Return expected and actual program output
-- | These are expected to pass and to be equal
genAssignCheck :: [(String,String)]
genAssignCheck = [asInt]
 where
    -- |Check integer literal assignment
    asInt = ("placeholder", "placeholder")
   
generateTest :: String -> String -> Test
generateTest e a = TestCase $ assertEqual "" e a

-- | Generate test cases
generateTests :: String -> [(String, String)] -> Test
generateTests s ps = makeLabels s tests
    where tests = map (uncurry generateTest) ps

  
-- | Test valid assignments 
assignGenTests :: Test
assignGenTests = generateTests "CodeGenAssignTest" genAssignCheck

-- | All Test cases to run
codeGenTests :: Test
codeGenTests = TestList [assignGenTests]
