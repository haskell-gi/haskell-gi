{-# LANGUAGE OverloadedStrings #-}

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
        ifProperties :: [Property],
        ifSignals :: [Signal],
        ifPrerequisites :: [Name],
        ifTypeInit :: Maybe Text,
        ifMethods :: [(Name, Method)],
        ifDeprecated :: Maybe DeprecationInfo
    } deriving Show

parseInterface :: Parser (Name, Interface)
parseInterface = do
  name <- parseName
  props <- parseChildrenWithLocalName "property" parseProperty
  signals <- parseChildrenWithNSName GLibGIRNS "signal" parseSignal
  prereqs <- parseChildrenWithLocalName "prerequisite" parseName
  typeInit <- queryAttrWithNamespace GLibGIRNS "get-type"
  methods <- parseChildrenWithLocalName "method" (parseMethod OrdinaryMethod)
  constructors <- parseChildrenWithLocalName "constructor" (parseMethod Constructor)
  deprecated <- parseDeprecation
  return (name,
         Interface {
            ifProperties = props
          , ifSignals = signals
          , ifPrerequisites = prereqs
          , ifTypeInit = typeInit
          , ifMethods = constructors ++ methods
          , ifDeprecated = deprecated
          })
