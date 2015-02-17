-- PlaceHolder Main--

import Data.List.Split
import System.Environment
import GHC.Conc (numCapabilities)

import GPC.Parser
import GPC.CodeGen
import GPC.Interpreter
import GPC.TypeScopeChecker
import GPC.AST
import GPC.SimplifyAST

outputCode :: FilePath -> String -> IO()
outputCode f s = writeFile f s --mapM_ putStrLn (lines s)

parseResult :: String -> Integer -> Either String (Program SrcPos) -> IO ()
parseResult f threads p = case p of
    Left err -> putStrLn err
    Right ast ->  do
        case runTypeChecker ast of
            Left err -> putStrLn err
            Right _ -> case genGPIR f (simplifyAST ast) threads of
               Left err -> putStrLn  err
               Right gpir -> do 
                    outputCode (f ++ ".td") $ ";" ++ f ++ ".yml\n" ++ (genCode gpir)
    

main = do
        args <- getArgs
        progName <- getProgName

        let file = head args
            filePrefix = (head $ splitOn "." file)
            threads = fromIntegral numCapabilities

        if length args <= 0
            then putStrLn ("Usage " ++ progName ++ " file")
            else (parseResult filePrefix threads) . parseSource =<< readFile file
