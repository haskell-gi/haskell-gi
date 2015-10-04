-- | Parsing of unions.
module GI.GIR.Union
    ( Union(..)
    , parseUnion
    ) where

import Data.Maybe (isJust)
import Data.Text (Text)

import GI.GIR.Field (Field, parseFields)
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
  constructors <- parseChildrenWithLocalName "constructor" (parseMethod Constructor)
  methods <- parseChildrenWithLocalName "method" (parseMethod OrdinaryMethod)
  return (name,
          Union {
            unionIsBoxed = isJust typeInit
          , unionTypeInit = typeInit
          , unionSize = error ("unfixed union size " ++ show name)
          , unionFields = fields
          , unionMethods = constructors ++ methods
          , unionDeprecated = deprecated
          })

