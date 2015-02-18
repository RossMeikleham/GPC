-- PlaceHolder Main--

import Data.List.Split
import Data.List
import Data.Maybe

import System.Environment
import GHC.Conc (numCapabilities)

import GPC.Parser
import GPC.CodeGen
import GPC.Interpreter
import GPC.TypeScopeChecker
import GPC.AST
import GPC.SimplifyAST

outputCode :: FilePath -> String -> IO()
outputCode f s = writeFile f s 

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
    
-- |Check if threads flag is set and try to obtain number
--  of threads to use
getThreads :: [String] -> Maybe Integer
getThreads args = do   
    arg <- listToMaybe $ filter (threadFlag `isPrefixOf`) args
    threads <- threadFlag `stripPrefix` arg 
    return $ ((read threads) :: Integer)
  where threadFlag = "--threads="    


main = do
        args <- getArgs
        progName <- getProgName

        let file = head args
            filePrefix = (head $ splitOn "." file)
            -- Set the number of threads in the system to the number specified
            -- in the arguments, if none is present detect number of threads in the system
            -- and use them
            threads = case getThreads args of
                        Nothing -> fromIntegral numCapabilities
                        Just n -> n

        if length args <= 0
            then putStrLn ("Usage " ++ progName ++ " file" ++ "[--threads=n]")
            else (parseResult filePrefix threads) . parseSource =<< readFile file






