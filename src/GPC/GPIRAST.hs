-- AST for GPIR

module GPC.GPIRAST (
    SymbolTree(..),
    GannetSymbol(..),
    OpSymbol(MkOpSymbol),
    Quoted
 )
where

data SymbolTree = Symbol GannetSymbol | SymbolList Quoted [SymbolTree] | EmptyTree deriving Show

-- (label LABEL ( ... ))    
data GannetSymbol = 
      ConstSymbol Quoted String
    | GOpSymbol OpSymbol 
--    | LabelKeywordSymbol Quoted String
--    | LabelSymbol Quoted String
    deriving Show

data OpSymbol = MkOpSymbol {
                        quoted  :: Quoted, -- ^ Whether symbol is quoted or not
                        node    :: (String, Int), -- ^ Node to run on
                        lib     :: String, -- ^ C++ library
                        gClass   :: String, -- ^ C++ class name
                        method  :: String  -- ^ C++ method call name
                    } deriving (Eq,Ord, Show)

type Quoted = Bool
