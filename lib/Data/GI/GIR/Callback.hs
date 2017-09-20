-- | Parsing of callbacks.
module Data.GI.GIR.Callback
    ( Callback(..)
    , parseCallback
    ) where

import Data.Text (Text)

import Data.GI.GIR.Callable (Callable, parseCallable)
import Data.GI.GIR.Parser
import Data.GI.GIR.Type (queryCType)

data Callback = Callback { cbCallable :: Callable
                         , cbCType    :: Maybe Text
                         , cbDocumentation :: Documentation
                         }
    deriving Show

parseCallback :: Parser (Name, Callback)
parseCallback = do
  name <- parseName
  callable <- parseCallable
  ctype <- queryCType
  doc <- parseDocumentation
  return (name, Callback { cbCallable = callable
                         , cbCType = ctype
                         , cbDocumentation = doc })
