-- PlaceHolder Main--

import Data.List.Split
import System.Environment
import GPC.Parser
import GPC.CodeGen
import GPC.GenGPIR
import GPC.TypeScopeChecker

outputCode :: FilePath -> String -> IO()
outputCode f s = writeFile f s --mapM_ putStrLn (lines s)

parseResult f p = case p of
    Left err -> print err
    Right v ->  case runTypeChecker v of
        Left err -> print err
        Right reduced -> case genGPIR reduced of
            Left err -> print err
            Right gpir -> print $ show gpir
    

main = do
        args <- getArgs
        progName <- getProgName

        let file = head args
            outFile = (head $ splitOn "." file) ++ ".td"

        if length args <= 0
            then putStrLn ("Usage " ++ progName ++ " file")
            else (parseResult outFile) . parseSource =<< readFile file
