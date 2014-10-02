{- Generate GPIR code from AST -}

module GPC.CodeGen (genCode) where

import GPC.AST

-- Generate GPIR code from Program AST
genCode :: Program -> String
genCode (Program xs) = concatMap (genTopLevel) xs

-- Generate code from Top Level statements
genTopLevel :: TopLevel -> String
genTopLevel tl = "placeholder"
