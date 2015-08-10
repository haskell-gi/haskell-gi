module GI.GIR.Function
    ( Function(..)
    , parseFunction
    ) where

import Text.XML (Element)

import GI.Internal.FunctionInfo (FunctionInfoFlag(..))
import GI.GIR.BasicTypes (ParseContext(..), Name(..))
import GI.GIR.Callable (Callable(..))

data Function = Function {
        fnSymbol :: String,
        fnCallable :: Callable,
        fnFlags :: [FunctionInfoFlag]
    } deriving Show

parseFunction :: ParseContext -> Element -> Maybe (Name, Function)
parseFunction _ _ = Nothing
