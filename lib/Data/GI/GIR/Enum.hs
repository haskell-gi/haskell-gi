-- | Parsing of Enums.
module Data.GI.GIR.Enum
    ( Enumeration(..)
    , EnumerationMember(..)
    , parseEnum
    ) where

import Data.Int (Int64)
import Data.Text (Text)
import Foreign.C (CInt(..))

import Data.GI.GIR.Parser
import Data.GI.GIR.Type (parseCType)

data Enumeration = Enumeration {
    enumMembers :: [EnumerationMember],
    enumErrorDomain :: Maybe Text,
    enumTypeInit :: Maybe Text,
    enumDocumentation :: Documentation,
    enumCType    :: Text,
    enumStorageBytes :: Int, -- ^ Bytes used for storage of this struct.
    enumDeprecated :: Maybe DeprecationInfo }
    deriving Show

-- | Member of an enumeration.
data EnumerationMember = EnumerationMember {
  enumMemberName   :: Text,
  enumMemberValue  :: Int64,
  enumMemberCId    :: Text,
  enumMemberDoc    :: Documentation
  } deriving Show

-- | Parse a struct member.
parseEnumMember :: Parser EnumerationMember
parseEnumMember = do
  name <- getAttr "name"
  value <- getAttr "value" >>= parseIntegral
  cid <- getAttrWithNamespace CGIRNS "identifier"
  doc <- parseDocumentation
  return $ EnumerationMember {
    enumMemberName = name,
    enumMemberValue = value,
    enumMemberCId = cid,
    enumMemberDoc = doc
    }

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
  ctype <- parseCType
  doc <- parseDocumentation
  deprecated <- parseDeprecation
  errorDomain <- queryAttrWithNamespace GLibGIRNS "error-domain"
  typeInit <- queryAttrWithNamespace GLibGIRNS "get-type"
  members <- parseChildrenWithLocalName "member" parseEnumMember
  return (name,
          Enumeration {
            enumMembers = members
          , enumErrorDomain = errorDomain
          , enumDocumentation = doc
          , enumTypeInit = typeInit
          , enumCType = ctype
          , enumStorageBytes = extractEnumStorageBytes (map enumMemberValue members)
          , enumDeprecated = deprecated
          })
