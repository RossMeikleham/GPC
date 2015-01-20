-- Simplify AST --

module GPC.SimplifyAST (simplifyAST) where

import qualified GPC.AST as A
import qualified GPC.TypelessAST as S

simplifyAST :: A.Program -> S.Program
simplifyAST A.Program tls = S.Program $ map simplifyTLStmt tls

simplifyTLStmt :: A.TopLevel -> S.TopLevel

