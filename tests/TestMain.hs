-- Main Test Suite, calls others --
import GPC.ParserTests

main :: IO Counts
main = do runTestTT parserTests 
