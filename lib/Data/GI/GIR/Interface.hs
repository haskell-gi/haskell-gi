module Data.GI.GIR.Interface
    ( Interface(..)
    , parseInterface
    ) where

import Data.Text (Text)

import Data.GI.GIR.Method (Method, MethodType(..), parseMethod)
import Data.GI.GIR.Property (Property, parseProperty)
import Data.GI.GIR.Signal (Signal, parseSignal)
import Data.GI.GIR.Parser
import Data.GI.GIR.Type (queryCType)

data Interface = Interface {
        ifTypeInit :: Maybe Text,
        ifCType :: Maybe Text,
        ifDocumentation :: Documentation,
        ifPrerequisites :: [Name],
        ifProperties :: [Property],
        ifSignals :: [Signal],
        ifMethods :: [Method],
        ifDeprecated :: Maybe DeprecationInfo
    } deriving Show

parseInterface :: Parser (Name, Interface)
parseInterface = do
  name <- parseName
  props <- parseChildrenWithLocalName "property" parseProperty
  signals <- parseChildrenWithNSName GLibGIRNS "signal" parseSignal
  typeInit <- queryAttrWithNamespace GLibGIRNS "get-type"
  methods <- parseChildrenWithLocalName "method" (parseMethod OrdinaryMethod)
  functions <- parseChildrenWithLocalName "function" (parseMethod MemberFunction)
  constructors <- parseChildrenWithLocalName "constructor" (parseMethod Constructor)
  deprecated <- parseDeprecation
  doc <- parseDocumentation
  ctype <- queryCType
  return (name,
         Interface {
            ifProperties = props
          , ifPrerequisites = error ("unfixed interface " ++ show name)
          , ifSignals = signals
          , ifTypeInit = typeInit
          , ifCType = ctype
          , ifDocumentation = doc
          , ifMethods = constructors ++ methods ++ functions
          , ifDeprecated = deprecated
          })
