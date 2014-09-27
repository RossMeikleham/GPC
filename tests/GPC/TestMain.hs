-- Main Test Suite, calls others --
import Test.HUnit

import GPC.ParserTests


main :: IO Counts
main = do runTestTT parserTests 
