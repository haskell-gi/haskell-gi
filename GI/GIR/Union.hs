{-# LANGUAGE OverloadedStrings #-}

-- | Parsing of unions.
module GI.GIR.Union
    ( Union(..)
    , parseUnion
    ) where

import Data.Maybe (isJust)
import Data.Text (Text)

import GI.Type (typeSize)

import GI.GIR.Field (Field(fieldType), parseFields)
import GI.GIR.Method (Method, MethodType(..), parseMethod)
import GI.GIR.Parser

data Union = Union {
    unionIsBoxed :: Bool,
    unionSize :: Int,
    unionTypeInit :: Maybe Text,
    unionFields :: [Field],
    unionMethods :: [(Name, Method)],
    unionDeprecated :: Maybe DeprecationInfo }
    deriving Show

parseUnion :: Parser (Name, Union)
parseUnion = do
  name <- parseName
  deprecated <- parseDeprecation
  typeInit <- queryAttrWithNamespace GLibGIRNS "get-type"
  fields <- parseFields
  let size = (maximum . map (typeSize . fieldType)) fields
  constructors <- parseChildrenWithLocalName "constructor" (parseMethod Constructor)
  methods <- parseChildrenWithLocalName "method" (parseMethod OrdinaryMethod)
  return (name,
          Union {
            --- XXX We need a better check to see whether a union is
            --- boxed, it could be that it is has a GType but it is
            --- not boxed... the only way I see of doing this is to
            --- load the library and ask.
            unionIsBoxed = isJust typeInit
          , unionTypeInit = typeInit
          , unionSize = size
          , unionFields = fields
          , unionMethods = constructors ++ methods
          , unionDeprecated = deprecated
          })

