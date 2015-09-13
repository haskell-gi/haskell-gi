module GI.GIR.Function
    ( Function(..)
    , FunctionInfoFlag(..)
    , parseFunction
    ) where

import Text.XML (Element)

import GI.GIR.BasicTypes (ParseContext(..), Name(..))
import GI.GIR.Callable (Callable(..))

data FunctionInfoFlag = FunctionIsMethod
                      | FunctionIsConstructor
                      | FunctionIsGetter
                      | FunctionIsSetter
                      | FunctionWrapsVFunc
                      | FunctionThrows
                        deriving (Show, Eq)

data Function = Function {
        fnSymbol :: String,
        fnCallable :: Callable,
        fnFlags :: [FunctionInfoFlag]
    } deriving Show

parseFunction :: ParseContext -> Element -> Maybe (Name, Function)
parseFunction _ _ = Nothing
