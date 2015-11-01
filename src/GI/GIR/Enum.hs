-- | Parsing of Enums.
module GI.GIR.Enum
    ( Enumeration(..)
    , parseEnum
    ) where

import Data.Int (Int64)
import Data.Text (Text)
import Foreign.C (CInt(..))

import GI.GIR.Parser

data Enumeration = Enumeration {
    enumValues :: [(Text, Int64)],
    errorDomain :: Maybe Text,
    enumTypeInit :: Maybe Text,
    enumStorageBytes :: Int, -- ^ Bytes used for storage of this struct.
    enumDeprecated :: Maybe DeprecationInfo }
    deriving Show

-- | Parse a struct member.
parseEnumMember :: Parser (Text, Int64)
parseEnumMember = do
  name <- getAttr "name"
  value <- getAttr "value" >>= parseIntegral
  return (name, value)

foreign import ccall "_gi_get_enum_storage_bytes" get_storage_bytes ::
    Int64 -> Int64 -> CInt

-- | Return the number of bytes that should be allocated for storage
-- of the given values in an enum.
extractEnumStorageBytes :: [Int64] -> Int
extractEnumStorageBytes values =
    fromIntegral $ get_storage_bytes (minimum values) (maximum values)

-- | Parse an "enumeration" element from the GIR file.
parseEnum :: Parser (Name, Enumeration)
parseEnum = do
  name <- parseName
  deprecated <- parseDeprecation
  errorDomain <- queryAttrWithNamespace GLibGIRNS "error-domain"
  typeInit <- queryAttrWithNamespace GLibGIRNS "get-type"
  values <- parseChildrenWithLocalName "member" parseEnumMember
  return (name,
          Enumeration {
            enumValues = values
          , errorDomain = errorDomain
          , enumTypeInit = typeInit
          , enumStorageBytes = extractEnumStorageBytes (map snd values)
          , enumDeprecated = deprecated
          })
