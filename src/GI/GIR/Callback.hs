-- | Parsing of callbacks.
module GI.GIR.Callback
    ( Callback(..)
    , parseCallback
    ) where

import GI.GIR.Parser
import GI.GIR.Callable (Callable, parseCallable)

data Callback = Callback Callable
    deriving Show

parseCallback :: Parser (Name, Callback)
parseCallback = do
  name <- parseName
  callable <- parseCallable
  return (name, Callback callable)
