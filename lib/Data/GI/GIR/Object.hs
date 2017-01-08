-- | Parsing of objects.
module Data.GI.GIR.Object
    ( Object(..)
    , parseObject
    ) where

import Data.Text (Text)

import Data.GI.GIR.Method (Method, parseMethod, MethodType(..))
import Data.GI.GIR.Property (Property, parseProperty)
import Data.GI.GIR.Signal (Signal, parseSignal)
import Data.GI.GIR.Parser
import Data.GI.GIR.Type (queryCType)

data Object = Object {
    objParent :: Maybe Name,
    objTypeInit :: Text,
    objTypeName :: Text,
    objCType :: Maybe Text,
    objInterfaces :: [Name],
    objDeprecated :: Maybe DeprecationInfo,
    objDocumentation :: Documentation,
    objMethods :: [Method],
    objProperties :: [Property],
    objSignals :: [Signal]
    } deriving Show

parseObject :: Parser (Name, Object)
parseObject = do
  name <- parseName
  deprecated <- parseDeprecation
  doc <- parseDocumentation
  methods <- parseChildrenWithLocalName "method" (parseMethod OrdinaryMethod)
  constructors <- parseChildrenWithLocalName "constructor" (parseMethod Constructor)
  functions <- parseChildrenWithLocalName "function" (parseMethod MemberFunction)
  parent <- optionalAttr "parent" Nothing (fmap Just . qualifyName)
  interfaces <- parseChildrenWithLocalName "implements" parseName
  props <- parseChildrenWithLocalName "property" parseProperty
  typeInit <- getAttrWithNamespace GLibGIRNS "get-type"
  typeName <- getAttrWithNamespace GLibGIRNS "type-name"
  signals <- parseChildrenWithNSName GLibGIRNS "signal" parseSignal
  ctype <- queryCType
  return (name,
         Object {
            objParent = parent
          , objTypeInit = typeInit
          , objCType = ctype
          , objTypeName = typeName
          , objInterfaces = interfaces
          , objDeprecated = deprecated
          , objDocumentation = doc
          , objMethods = constructors ++ methods ++ functions
          , objProperties = props
          , objSignals = signals
          })

