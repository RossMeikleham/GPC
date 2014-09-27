module TestMain where 

-- Main Test Suite, calls others --
import Test.HUnit
import qualified Distribution.TestSuite as TS
import GPC.ParserTests

allTests :: Test 
allTests = TestList [parserTests] 

-- |Need to convert HUnit test results to Cabal test suite so
-- |our tests can be run with cabal
convertToCabalTests :: IO TS.Progress
convertToCabalTests = do 
    (Counts _ _ er fl) <- hunitTests
    testResult er fl
 where 
    testResult e f
        | e > 0 = return $ TS.Finished $ TS.Error 
              "An error occured running one or more tests"
        | f > 0 = return $ TS.Finished $ TS.Fail 
              "One of more tests failed"
        | otherwise  = return $ TS.Finished $ TS.Pass
   
-- |Run HUnit tests
hunitTests :: IO Counts
hunitTests = runTestTT allTests


-- |Execute all tests and report results
tests :: IO [TS.Test]
tests = return [TS.Test hunit]
 where hunit = TS.TestInstance {
     TS.run = convertToCabalTests
    ,TS.name = "Gannet Test Cases"
    ,TS.tags = ["Gannet"]
    ,TS.options = []
    ,TS.setOption = \_ _ -> Right hunit
    }
