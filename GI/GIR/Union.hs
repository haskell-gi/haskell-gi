{-# LANGUAGE OverloadedStrings #-}

-- | Parsing of unions.
module GI.GIR.Union
    ( Union(..)
    , parseUnion
    ) where

import Data.Text (Text)
import Text.XML (Element)

import GI.GIR.BasicTypes (ParseContext, Name)
import GI.GIR.Deprecation (DeprecationInfo)

import GI.GIR.Function (Function)
import GI.GIR.Field (Field)

-- XXX: Capture alignment and method info.

data Union = Union {
    unionIsBoxed :: Bool,
    unionSize :: Int,
    unionTypeInit :: Maybe Text,
    unionFields :: [Field],
    unionMethods :: [(Name, Function)],
    unionDeprecated :: Maybe DeprecationInfo }
    deriving Show

{-
toUnion :: UnionInfo -> Union
toUnion ui = Union
    (isJust (registeredTypeInfoTypeInit ui) &&
        gtypeIsBoxed (registeredTypeInfoGType ui))
    (unionInfoSize ui)
    (registeredTypeInfoTypeInit ui)
    (map toField $ unionInfoFields ui)
    (map (withName toFunction) (unionInfoMethods ui))
    (infoDeprecated ui)
-}

parseUnion :: ParseContext -> Element -> Maybe (Name, Union)
parseUnion _ _ = Nothing

