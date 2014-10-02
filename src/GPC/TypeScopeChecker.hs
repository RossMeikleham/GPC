{- Check types and scope of identifiers -}

import Control.Monad.State.Lazy
import qualified Data.Map as M
import GPC.AST

scopes = M.Map Int -> 

data Block = Block {
    types :: [String] -- ^ Current types defined
    funcDefs :: M.Map String [String] -- ^ Function names and argument types
    prevIdentifiers :: M.Map String String -- ^ Identifiers from previous scope above with types
    curIdentifiers :: M.Map String String -- ^ Identifiers in the current scope
    statements :: [Stmt] -- ^ Current statements in the block


createBlock :: [Stmt] -> Block -> Block
createBlock =     
