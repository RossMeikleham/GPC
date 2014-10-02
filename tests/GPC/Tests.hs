{- Common Test functions used in test modules -}
module GPC.Tests (makeLabels) where

import Test.HUnit

-- | Make labels for test cases given a string for example given name
-- | assign, creates labels assign1 assign2 etc for tests
makeLabels :: String -> [Test] -> Test
makeLabels str tests = TestList $ zipWith TestLabel labels tests
 where labels = map(str ++) nums
       nums = map showInt [1..]
       showInt :: Int -> String
       showInt = show

