-- | Parsing of callbacks.
module Data.GI.GIR.Callback
    ( Callback(..)
    , parseCallback
    ) where

import Data.GI.GIR.Parser
import Data.GI.GIR.Callable (Callable, parseCallable)

data Callback = Callback Callable
    deriving Show

parseCallback :: Parser (Name, Callback)
parseCallback = do
  name <- parseName
  callable <- parseCallable
  return (name, Callback callable)
