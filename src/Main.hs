-- PlaceHolder Main--

import Data.List.Split
import System.Environment
import GPC.Parser
--import GPC.CodeGen
--import GPC.GenGPIR
import GPC.TypeScopeChecker
import GPC.AST
import GPC.SimplifyAST
import qualified GPC.TypelessAST as S

outputCode :: FilePath -> String -> IO()
outputCode f s = writeFile f s --mapM_ putStrLn (lines s)

parseResult :: String -> Either String (Program SrcPos) -> IO ()
parseResult f p = case p of
    Left err -> print err
    Right v ->  do 
        case runTypeChecker v of
            Left err -> print err
            Right reduced -> print reduced --case genGPIR f reduced of
               -- Left err -> print err
               -- Right gpir -> do 
               --     outputCode (f ++ ".td") $ ";" ++ f ++ ".yml\n" ++ (genCode gpir)
    

main = do
        args <- getArgs
        progName <- getProgName

        let file = head args
            filePrefix = (head $ splitOn "." file)

        if length args <= 0
            then putStrLn ("Usage " ++ progName ++ " file")
            else (parseResult filePrefix) . parseSource =<< readFile file
