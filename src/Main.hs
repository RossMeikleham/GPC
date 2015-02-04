-- PlaceHolder Main--

import Data.List.Split
import System.Environment
import GPC.Parser
import GPC.CodeGen
import GPC.Interpreter
import GPC.TypeScopeChecker
import GPC.AST
import GPC.SimplifyAST

outputCode :: FilePath -> String -> IO()
outputCode f s = writeFile f s --mapM_ putStrLn (lines s)

parseResult :: String -> Either String (Program SrcPos) -> IO ()
parseResult f p = case p of
    Left err -> print err
    Right ast ->  do
        case runTypeChecker ast of
            Left err -> print err
            Right _ -> case genGPIR f (simplifyAST ast) of
               Left err -> print err
               Right gpir -> do 
                    outputCode (f ++ ".td") $ ";" ++ f ++ ".yml\n" ++ (genCode gpir)
    

main = do
        args <- getArgs
        progName <- getProgName

        let file = head args
            filePrefix = (head $ splitOn "." file)

        if length args <= 0
            then putStrLn ("Usage " ++ progName ++ " file")
            else (parseResult filePrefix) . parseSource =<< readFile file
