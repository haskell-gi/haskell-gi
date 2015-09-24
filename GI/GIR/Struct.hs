{-# LANGUAGE OverloadedStrings #-}

-- | Parsing of structs.
module GI.GIR.Struct
    ( Struct(..)
    , parseStruct
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Data.Maybe (isJust)
import Data.Text (Text)

import GI.GIR.Field (Field, parseFields, computeFieldOffsets)
import GI.GIR.Method (Method, MethodType(..), parseMethod)
import GI.GIR.Parser

data Struct = Struct {
    structIsBoxed :: Bool,
    structTypeInit :: Maybe Text,
    structSize :: Int,
    gtypeStructFor :: Maybe Name,
    -- https://bugzilla.gnome.org/show_bug.cgi?id=560248
    structIsDisguised :: Bool,
    structFields :: [Field],
    structMethods :: [(Name, Method)],
    structDeprecated :: Maybe DeprecationInfo }
    deriving Show

parseStruct :: Parser (Name, Struct)
parseStruct = do
  name <- parseName
  deprecated <- parseDeprecation
  structFor <- queryAttrWithNamespace GLibGIRNS "is-gtype-struct-for" >>= \case
               Just t -> (fmap Just . qualifyName) t
               Nothing -> return Nothing
  typeInit <- queryAttrWithNamespace GLibGIRNS "get-type"
  disguised <- optionalAttr "disguised" False parseBool
  (fields, size) <- computeFieldOffsets <$> parseFields
  constructors <- parseChildrenWithLocalName "constructor" (parseMethod Constructor)
  methods <- parseChildrenWithLocalName "method" (parseMethod OrdinaryMethod)
  return (name,
          Struct {
            --- XXX We need a better check to see whether a struct is
            --- boxed, it could be that it is has a GType but it is
            --- not boxed... the only way I see of doing this is to
            --- load the library and ask.
            structIsBoxed = isJust typeInit
          , structTypeInit = typeInit
          , structSize = size
          , gtypeStructFor = structFor
          , structIsDisguised = disguised
          , structFields = fields
          , structMethods = constructors ++ methods
          , structDeprecated = deprecated
          })
