{- Check types and scope of identifiers -}

import Control.Monad.State.Lazy
import qualified Data.Map as M
import GPC.AST


builtInTypes = ["int", "float", "char", "string", "bool"]

data CodeBlock = CodeBlock {
    types :: [String], -- ^ Current types defined
    funcDefs :: M.Map String (Type, [Type]), -- ^ Function names and return/argument types
    prevIdentifiers :: M.Map String String, -- ^ Identifiers from previous scope above with types
    curIdentifiers :: M.Map String String, -- ^ Identifiers in the current scope
    subBlocks :: [CodeBlock],
    statements :: [Stmt] -- ^ Current statements in the block
}


initialBlock :: CodeBlock
initialBlock = CodeBlock builtInTypes M.empty M.empty M.empty [] []
    

isFun (Func _ _ _ _) = True
isFun (TLObjs _) = False
isFun (TlAssign _) = False

createBlocks :: Program -> CodeBlock
createBlocks (Program xs) = initialBlock
 where funs = filter isFun xs
        -- Generate Function Definition Map
       funDefs = map (\(Func (Type t) (Ident name) xs _) ->
                    M.singleton name $ (t, map (fst) xs)) funs


{-
checkConstExprTypes :: ConstExpr -> Type
checkConstExprTypes (ConstBinOp And e1 e2) = 
checkConstExprTypes (ConstBinOp Or ) =
checkConstExprTypes (ConstUnaryOp Not e) =-}

    
