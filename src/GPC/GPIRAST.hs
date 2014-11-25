-- AST for GPIR

module GPC.GPIRAST (
    SymbolTree(..),
    GannetSymbol(..),
    OpSymbol(..),
    Quoted
 )
where

data SymbolTree = Symbol GannetSymbol | SymbolList Quoted [SymbolTree]

-- (label LABEL ( ... ))    
data GannetSymbol = 
      ConstSymbol Quoted String
    | GOpSymbol OpSymbol 
    | LabelKeywordSymbol Quoted String
    | LabelSymbol Quoted String


data OpSymbol = MkOpSymbol {
                        quoted  :: Quoted, -- ^ Whether symbol is quoted or not
                        node    :: (String, Int), -- ^ Node to run on
                        lib     :: String, -- ^ C++ library
                        gClass   :: String, -- ^ C++ class name
                        method  :: String  -- ^ C++ method call name
                    } deriving (Eq,Ord)

type Quoted = Bool
