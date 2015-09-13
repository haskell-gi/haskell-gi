{-# LANGUAGE OverloadedStrings #-}

-- | Parsing of callbacks.
module GI.GIR.Callback
    ( Callback(..)
    , parseCallback
    ) where

import Text.XML (Element)
import GI.GIR.BasicTypes (ParseContext, Name)

import GI.GIR.Callable (Callable)

-- XXX
data Callback = Callback Callable
    deriving Show

{-
toCallback = Callback . toCallable
-}

parseCallback :: ParseContext -> Element -> Maybe (Name, Callback)
parseCallback _ _ = Nothing
