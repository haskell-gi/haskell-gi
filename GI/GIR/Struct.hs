{-# LANGUAGE OverloadedStrings #-}

-- | Parsing of structs.
module GI.GIR.Struct
    ( Struct(..)
    , parseStruct
    ) where

import Data.Text (Text)
import Text.XML (Element)

import GI.GIR.BasicTypes (ParseContext, Name)
import GI.GIR.Deprecation (DeprecationInfo)

import GI.GIR.Function (Function)
import GI.GIR.Field (Field)

data Struct = Struct {
    structIsBoxed :: Bool,
    structTypeInit :: Maybe Text,
    structSize :: Int,
    structIsForeign :: Bool,
    isGTypeStruct :: Bool,
    -- https://bugzilla.gnome.org/show_bug.cgi?id=560248
    structIsDisguised :: Bool,
    structFields :: [Field],
    structMethods :: [(Name, Function)],
    structDeprecated :: Maybe DeprecationInfo }
    deriving Show

{-
toStruct :: StructInfo -> Struct
toStruct si = Struct
    (isJust (registeredTypeInfoTypeInit si) &&
        gtypeIsBoxed (registeredTypeInfoGType si))
    (registeredTypeInfoTypeInit si)
    (structInfoSize si)
    (structInfoIsForeign si)
    (structInfoIsGTypeStruct si)
    (map toField $ structInfoFields si)
    (map (withName toFunction) (structInfoMethods si))
    (infoDeprecated si)
-}

parseStruct :: ParseContext -> Element -> Maybe (Name, Struct)
parseStruct _ _ = Nothing
