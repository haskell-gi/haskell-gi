module Data.GI.GIR.Signal
    ( Signal(..)
    , parseSignal
    ) where

import Data.Text (Text)

import Data.GI.GIR.Callable (Callable(..), parseCallable)
import Data.GI.GIR.Parser

data Signal = Signal {
        sigName :: Text,
        sigCallable :: Callable,
        sigDeprecated :: Maybe DeprecationInfo
    } deriving (Show, Eq)

parseSignal :: Parser Signal
parseSignal = do
  n <- getAttr "name"
  deprecated <- parseDeprecation
  callable <- parseCallable
  return $ Signal {
                sigName = n
              , sigCallable = callable
              , sigDeprecated = deprecated
              }
