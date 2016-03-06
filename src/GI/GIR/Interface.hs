module GI.GIR.Interface
    ( Interface(..)
    , parseInterface
    ) where

import Data.Text (Text)

import GI.GIR.Method (Method, MethodType(..), parseMethod)
import GI.GIR.Property (Property, parseProperty)
import GI.GIR.Signal (Signal, parseSignal)
import GI.GIR.Parser

data Interface = Interface {
        ifTypeInit :: Maybe Text,
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
  return (name,
         Interface {
            ifProperties = props
          , ifPrerequisites = error ("unfixed interface " ++ show name)
          , ifSignals = signals
          , ifTypeInit = typeInit
          , ifMethods = constructors ++ methods ++ functions
          , ifDeprecated = deprecated
          })
