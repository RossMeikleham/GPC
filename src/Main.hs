-- PlaceHolder Main--

import System.Environment
import GPC.Parser
import GPC.CodeGen

outputCode :: String -> IO()
outputCode s = mapM_ putStrLn (lines s)

parseResult p = case p of
    Left err -> print err
    Right v -> outputCode $ genCode v
    

main = do
        args <- getArgs
        progName <- getProgName

        let file = head args

        if length args <= 0
            then putStrLn ("Usage " ++ progName ++ " file")
            else parseResult . parseSource =<< readFile file
