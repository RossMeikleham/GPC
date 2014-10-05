-- PlaceHolder Main--

import System.Environment
import GPC.Parser
import GPC.CodeGen

parseResult p = case p of
    Left err -> print err
    Right v -> print $ genCode v
    

main = do
        args <- getArgs
        progName <- getProgName

        let file = head args

        if length args <= 0
            then putStrLn ("Usage " ++ progName ++ " file")
            else parseResult . parseSource =<< readFile file
