{- Error Types -}

module GPC.Errors where

import GPC.AST
import Control.Monad.Error

data TypeScopeError = 
     NotInScope (Ident SrcPos) -- ^ Identifier not in scope
   | DuplicateArgNames (Ident SrcPos) (Ident SrcPos) -- ^ Duplicate Argument names in given Function
   | MultipleFunctionDecl (Ident SrcPos) -- ^ Multiple Functions of the same name declared
   | TypeMismatch (Type SrcPos) (Type SrcPos) -- ^ Type mismatch, expected, actual
   | MultipleInstances (Ident SrcPos) -- ^ Multiple instances of an identifier in current scope
   | OtherError String 

instance Show TypeScopeError where
    show (NotInScope i) = 
        errorIdent i ++ "Identifier \"" ++ (show i) ++ "\" not in scope" 

    show (DuplicateArgNames fI aI) = 
        errorIdent aI ++ "Duplicate instance of argument \"" ++ (show aI) ++
                         "\" in function definition for function \"" ++ (show fI) ++ "\"."

    show (MultipleFunctionDecl i) = 
        "Multiple definitions of function \"" ++ (show i) ++ "\"."

    show (TypeMismatch expected actual) = 
        errorType expected ++ "Expected type " ++ show expected ++ 
                              " but expression evaluated to type " ++ show actual ++ "."

    show (MultipleInstances i) = 
        errorIdent i ++ "Multiple instances of \"" ++ show i ++ 
                        "\" in current scope."

    show (OtherError s) = s


instance Error TypeScopeError where
    noMsg = OtherError "Type/Scope error!"
    strMsg s = OtherError s


errorSrcPos :: SrcPos -> String
errorSrcPos (SrcPos line col) = "Error line:" ++ show line ++ " column:" ++ show col ++ "\n"

errorIdent :: Ident SrcPos -> String
errorIdent (Ident sp _) = errorSrcPos sp

errorType :: Type SrcPos -> String
errorType (PointerType t) = errorType t
errorType (NormalType sp _ _) = errorSrcPos sp
