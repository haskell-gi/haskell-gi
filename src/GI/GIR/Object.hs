-- | Parsing of objects.
module GI.GIR.Object
    ( Object(..)
    , parseObject
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Data.Text (Text)

import GI.GIR.Method (Method, parseMethod, MethodType(..))
import GI.GIR.Property (Property, parseProperty)
import GI.GIR.Signal (Signal, parseSignal)
import GI.GIR.Parser

data Object = Object {
    objParent :: Maybe Name,
    objTypeInit :: Text,
    objTypeName :: Text,
    objInterfaces :: [Name],
    objDeprecated :: Maybe DeprecationInfo,
    objMethods :: [(Name, Method)],
    objProperties :: [Property],
    objSignals :: [Signal]
    } deriving Show

parseObject :: Parser (Name, Object)
parseObject = do
  name <- parseName
  deprecated <- parseDeprecation
  methods <- parseChildrenWithLocalName "method" (parseMethod OrdinaryMethod)
  constructors <- parseChildrenWithLocalName "constructor" (parseMethod Constructor)
  functions <- parseChildrenWithLocalName "function" (parseMethod MemberFunction)
  parent <- optionalAttr "parent" Nothing (fmap Just . qualifyName)
  interfaces <- parseChildrenWithLocalName "implements" parseName
  props <- parseChildrenWithLocalName "property" parseProperty
  typeInit <- getAttrWithNamespace GLibGIRNS "get-type"
  typeName <- getAttrWithNamespace GLibGIRNS "type-name"
  signals <- parseChildrenWithNSName GLibGIRNS "signal" parseSignal
  return (name,
         Object {
            objParent = parent
          , objTypeInit = typeInit
          , objTypeName = typeName
          , objInterfaces = interfaces
          , objDeprecated = deprecated
          , objMethods = constructors ++ methods ++ functions
          , objProperties = props
          , objSignals = signals
          })

