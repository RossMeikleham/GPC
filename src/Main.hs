-- PlaceHolder Main--

import Data.List.Split
import System.Environment
import GPC.Parser
import GPC.CodeGen

outputCode :: FilePath -> String -> IO()
outputCode f s = writeFile f s --mapM_ putStrLn (lines s)

parseResult f p = case p of
    Left err -> print err
    Right v -> outputCode f $ genCode v
    

main = do
        args <- getArgs
        progName <- getProgName

        let file = head args
            outFile = (head $ splitOn "." file) ++ ".td"

        if length args <= 0
            then putStrLn ("Usage " ++ progName ++ " file")
            else (parseResult outFile) . parseSource =<< readFile file
