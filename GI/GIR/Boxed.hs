{-# LANGUAGE OverloadedStrings #-}

-- | Parsing of structs.
module GI.GIR.Boxed
    ( Boxed(..)
    , parseBoxed
    ) where

import Text.XML (Element)

import GI.GIR.BasicTypes (ParseContext, Name)

-- XXX: Work out what to do with boxed types.
data Boxed = Boxed
    deriving Show

parseBoxed :: ParseContext -> Element -> Maybe (Name, Boxed)
parseBoxed _ _ = Nothing
