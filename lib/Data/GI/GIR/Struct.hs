-- | Parsing of structs.
module Data.GI.GIR.Struct
    ( Struct(..)
    , parseStruct
    ) where

import Data.Text (Text)

import Data.GI.GIR.Allocation (AllocationInfo(..), unknownAllocationInfo)
import Data.GI.GIR.Field (Field, parseFields)
import Data.GI.GIR.Method (Method, MethodType(..), parseMethod)
import Data.GI.GIR.Parser

data Struct = Struct {
    structIsBoxed :: Bool,
    structAllocationInfo :: AllocationInfo,
    structTypeInit :: Maybe Text,
    structSize :: Int,
    gtypeStructFor :: Maybe Name,
    -- https://bugzilla.gnome.org/show_bug.cgi?id=560248
    structIsDisguised :: Bool,
    structFields :: [Field],
    structMethods :: [Method],
    structDeprecated :: Maybe DeprecationInfo,
    structDocumentation :: Maybe Documentation }
    deriving Show

parseStruct :: Parser (Name, Struct)
parseStruct = do
  name <- parseName
  deprecated <- parseDeprecation
  doc <- parseDocumentation
  structFor <- queryAttrWithNamespace GLibGIRNS "is-gtype-struct-for" >>= \case
               Just t -> (fmap Just . qualifyName) t
               Nothing -> return Nothing
  typeInit <- queryAttrWithNamespace GLibGIRNS "get-type"
  disguised <- optionalAttr "disguised" False parseBool
  fields <- parseFields
  constructors <- parseChildrenWithLocalName "constructor" (parseMethod Constructor)
  methods <- parseChildrenWithLocalName "method" (parseMethod OrdinaryMethod)
  functions <- parseChildrenWithLocalName "function" (parseMethod MemberFunction)
  return (name,
          Struct {
            structIsBoxed = error ("[boxed] unfixed struct " ++ show name)
          , structAllocationInfo = unknownAllocationInfo
          , structTypeInit = typeInit
          , structSize = error ("[size] unfixed struct " ++ show name)
          , gtypeStructFor = structFor
          , structIsDisguised = disguised
          , structFields = fields
          , structMethods = constructors ++ methods ++ functions
          , structDeprecated = deprecated
          , structDocumentation = doc
          })
