{- Generate .td file from GPIR AST -}

module GPC.CodeGen (genCode) where

import Text.PrettyPrint.Leijen 
import GPC.GPIRAST

nestLevel = 4 -- |Number of spaces to nest

genCode :: SymbolTree -> String
genCode st = show $ genCode' st False

genCode' :: SymbolTree -> Quoted -> Doc
genCode' (Symbol gSymbol) carryQuote = case gSymbol of
    ConstSymbol quoted str -> text (strQuoted ++ str) 
     where strQuoted = if quoted || carryQuote then "'"  else ""

    GOpSymbol (MkOpSymbol quoted _ method) ->
        text $ strQuoted ++ method' method
     where strQuoted = if quoted || carryQuote then "'" else "" 
           method' = foldl1 (\a b -> a ++ "." ++ b)

-- | Generate Source Code for Symbol Tree
genCode' (SymbolList quoted symbolTree) carryQuote = case symbolTree of
    [] -> text ""
    (x:[]) -> genCode' x quoted -- If tree contains 1 element, carry the quote over
    xs -> parens'(quoted || carryQuote) $ 
            foldl1 (<+>) (map (`genCode'` False) xs)


parens' :: Quoted -> Doc -> Doc 
parens' q x = nest' nestLevel $ quoteText <> parens x
 where quoteText = text $ if q then "'" else ""

nest' :: Int -> Doc -> Doc
nest' n d = text "" <$> nest n d
