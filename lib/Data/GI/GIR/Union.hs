-- | Parsing of unions.
module Data.GI.GIR.Union
    ( Union(..)
    , parseUnion
    ) where

import Data.Maybe (isJust)
import Data.Text (Text)

import Data.GI.GIR.Allocation (AllocationInfo(..), unknownAllocationInfo)
import Data.GI.GIR.Field (Field, parseFields)
import Data.GI.GIR.Method (Method, MethodType(..), parseMethod)
import Data.GI.GIR.Parser
import Data.GI.GIR.Type (queryCType)

data Union = Union {
    unionIsBoxed :: Bool,
    unionAllocationInfo :: AllocationInfo,
    unionDocumentation :: Documentation,
    unionSize :: Int,
    unionTypeInit :: Maybe Text,
    unionFields :: [Field],
    unionMethods :: [Method],
    unionCType :: Maybe Text,
    unionDeprecated :: Maybe DeprecationInfo }
    deriving Show

parseUnion :: Parser (Name, Union)
parseUnion = do
  name <- parseName
  deprecated <- parseDeprecation
  doc <- parseDocumentation
  typeInit <- queryAttrWithNamespace GLibGIRNS "get-type"
  fields <- parseFields
  constructors <- parseChildrenWithLocalName "constructor" (parseMethod Constructor)
  methods <- parseChildrenWithLocalName "method" (parseMethod OrdinaryMethod)
  functions <- parseChildrenWithLocalName "function" (parseMethod MemberFunction)
  ctype <- queryCType

  return (name,
          Union {
            unionIsBoxed = isJust typeInit
          , unionAllocationInfo = unknownAllocationInfo
          , unionDocumentation = doc
          , unionTypeInit = typeInit
          , unionSize = error ("unfixed union size " ++ show name)
          , unionFields = fields
          , unionMethods = constructors ++ methods ++ functions
          , unionCType = ctype
          , unionDeprecated = deprecated
          })

