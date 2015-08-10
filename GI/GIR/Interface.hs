{-# LANGUAGE OverloadedStrings #-}

module GI.GIR.Interface
    ( Interface(..)
    , parseInterface
    ) where

import Data.Maybe (mapMaybe)
import Text.XML (Element)

import GI.GIR.BasicTypes (ParseContext, Name, nameInCurrentNS)
import GI.GIR.Constant (Constant(..))
import GI.GIR.Deprecation (DeprecationInfo)
import GI.GIR.Function (Function(..), parseFunction)
import GI.GIR.Property (Property(..))
import GI.GIR.Signal (Signal(..))
import GI.GIR.XMLUtils (lookupAttr, childElemsWithLocalName)

data Interface = Interface {
        ifConstants :: [(Name, Constant)],
        ifProperties :: [Property],
        ifSignals :: [Signal],
        ifPrerequisites :: [Name],
        ifTypeInit :: Maybe String,
        ifMethods :: [(Name, Function)],
        ifDeprecated :: Maybe DeprecationInfo
    } deriving Show

parseInterface :: ParseContext -> Element -> Maybe (Name, Interface)
parseInterface ctx element = do
    name <- lookupAttr "name" element
    let methods = mapMaybe (parseFunction ctx) $
            childElemsWithLocalName "method" element
    return (nameInCurrentNS ctx name, Interface {
            ifConstants = [],
            ifProperties = [],
            ifSignals = [],
            ifPrerequisites = [],
            ifTypeInit = Nothing,
            ifMethods = methods,
            ifDeprecated = Nothing
        })
