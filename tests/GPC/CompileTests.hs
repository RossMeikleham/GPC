-- Attempt to run the entire compiler on
-- source files, check files in the pass file
-- successfully compile, and that fails in the
-- fail folder, fail to compile at some point

module GPC.CompileTests (compileTests) where

import qualified Test.Framework.Providers.API as TFA
import Test.Framework.Providers.HUnit
import Test.HUnit
import System.Directory

import Data.List

import GPC.Parser
import GPC.TypeScopeChecker
import GPC.GenGPIR

passDir =  "tests/examples/pass/"
failDir =  "tests/examples/fail/"

sourceFiles :: String -> IO [String]
sourceFiles dir = do 
    files <- getDirectoryContents dir
    return $ filter (isSuffixOf ".gpc") files


passTests :: IO TFA.Test
passTests = do 
    filePaths <- sourceFiles passDir
    let tests = map passTest filePaths
    return $ TFA.testGroup "Pass Tests" tests

passTest :: String -> TFA.Test
passTest file = testCase file ( do
    source <- readFile filePath
    case parseSource source of
        Left err -> assertFailure err
        Right v ->  do 
            case runTypeChecker v of
                Left err -> assertFailure err
                Right reduced -> return () -- case genGPIR fileName reduced of
                   -- Left err -> assertFailure err
                   -- Right _ -> return ()
    )
 where
    filePath = passDir ++ file 
    fileName = reverse $ drop 4 $ reverse file

failTests :: IO TFA.Test
failTests = do 
    filePaths <- sourceFiles failDir
    let tests = map failTest filePaths
    return $ TFA.testGroup "Fail Tests" tests

failTest :: String -> TFA.Test
failTest file = testCase file ( do
    source <- readFile filePath
    case parseSource source of
        Left _ -> return () 
        Right v ->  do 
            case runTypeChecker v of
                Left _ -> return ()
                Right reduced -> assertFailure $ "Program shouldn't have type checked" ++ show reduced --case genGPIR fileName reduced of
                   -- Left _ -> return ()
                   -- Right _ -> assertFailure "Program shouldn't have compiled"
    )
 where
    filePath = failDir ++ file 
    fileName = reverse $ drop 4 $ reverse file

compileTests =  
    TFA.testGroup "Compiler Tests" $ map TFA.buildTest [passTests, failTests]
