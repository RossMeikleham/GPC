{- Building blocks from AST -}

import Control.Monad.State.Lazy
import qualified Data.Map as M
import GPC.AST

data Block = Block {
    types :: [String] -- | Current types defined
    funcDefs :: M.Map String [String] -- | Function names and argument types
    identifiers :: M.Map String String -- | Current identifiers in scope with types


createBlock :: [Stmt] -> Block -> Block
createBlock =     
