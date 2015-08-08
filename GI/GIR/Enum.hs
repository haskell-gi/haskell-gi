{-# LANGUAGE OverloadedStrings #-}

-- | Parsing of Enums.
module GI.GIR.Enum
    ( Enumeration(..)
    , parseEnum
    ) where

import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Text.XML (Element)
import Foreign.C (CInt(..))

import GI.GIR.BasicTypes (ParseContext, Name, nameInCurrentNS)
import GI.GIR.Deprecation (DeprecationInfo, parseDeprecation)
import GI.GIR.XMLUtils (subelements, parseIntegral, lookupAttr,
                        lookupAttrWithNamespace, GIRXMLNamespace(GLibGIRNS))

data Enumeration = Enumeration {
    enumValues :: [(Text, Int64)],
    errorDomain :: Maybe Text,
    enumTypeInit :: Maybe Text,
    enumStorageBytes :: Int, -- ^ Bytes used for storage of this struct.
    enumDeprecated :: Maybe DeprecationInfo }
    deriving Show

-- | Parse a struct member.
parseEnumMember :: Element -> Maybe (Text, Int64)
parseEnumMember element = do
  name <- lookupAttr "name" element
  value <- lookupAttr "value" element
  intValue <- parseIntegral value
  return (name, intValue)

foreign import ccall "_gi_get_enum_storage_bytes" get_storage_bytes ::
    Int64 -> Int64 -> CInt

-- | Return the number of bytes that should be allocated for storage
-- of the given values in an enum.
extractEnumStorageBytes :: [Int64] -> Int
extractEnumStorageBytes values =
    fromIntegral $ get_storage_bytes (minimum values) (maximum values)

-- | Parse an "enumeration" element from the GIR file.
parseEnum :: ParseContext -> Element -> Maybe (Name, Enumeration)
parseEnum ctx element = do
  name <- lookupAttr "name" element
  let values = mapMaybe parseEnumMember (subelements element)
  return (nameInCurrentNS ctx name,
          Enumeration {
            enumValues = values
          , errorDomain = lookupAttrWithNamespace GLibGIRNS "error-domain" element
          , enumTypeInit = lookupAttrWithNamespace GLibGIRNS "get-type" element
          , enumStorageBytes = extractEnumStorageBytes (map snd values)
          , enumDeprecated = parseDeprecation element
          })
