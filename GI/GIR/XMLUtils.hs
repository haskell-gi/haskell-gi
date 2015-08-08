{-# LANGUAGE OverloadedStrings #-}
-- | Some helpers for making traversals of GIR documents easier.
module GI.GIR.XMLUtils
    ( nodeToElement
    , subelements
    , localName
    , lookupAttr
    , GIRXMLNamespace(..)
    , lookupAttrWithNamespace
    , childElemsWithLocalName
    , firstChildWithLocalName
    , getElementContent
    , parseIntegral
    ) where

import Text.XML (Element(elementNodes, elementName, elementAttributes),
                 Node(NodeContent, NodeElement), nameLocalName, Name(..))
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Read as TR

-- | Turn a node into an element (if it is indeed an element node).
nodeToElement :: Node -> Maybe Element
nodeToElement (NodeElement e) = Just e
nodeToElement _               = Nothing

-- | Find all children of the given element which are XML Elements
-- themselves.
subelements :: Element -> [Element]
subelements = mapMaybe nodeToElement . elementNodes

-- | The local name of an element.
localName :: Element -> Text
localName = nameLocalName . elementName

-- | Restrict to those with the given local name.
childElemsWithLocalName :: Text -> Element -> [Element]
childElemsWithLocalName n =
    filter localNameMatch . subelements
    where localNameMatch = (== n) . localName

-- | Find the first child element with the given name.
firstChildWithLocalName :: Text -> Element -> Maybe Element
firstChildWithLocalName n = listToMaybe . childElemsWithLocalName n

-- | Get the content of a given element, if it exists.
getElementContent :: Element -> Maybe Text
getElementContent = listToMaybe . mapMaybe getContent . elementNodes
    where getContent :: Node -> Maybe Text
          getContent (NodeContent t) = Just t
          getContent _ = Nothing

-- | Parse a signed integral number.
parseIntegral :: Integral a => Text -> Maybe a
parseIntegral str = case TR.signed TR.decimal str of
                     Right (n, r) | T.null r -> Just n
                     _ -> Nothing

-- | Lookup an attribute for an element (with no prefix).
lookupAttr :: Name -> Element -> Maybe Text
lookupAttr attr element = M.lookup attr (elementAttributes element)

-- | GIR namespaces we know about.
data GIRXMLNamespace = GLibGIRNS | CGIRNS

-- | Return the text representation of the known GIR namespaces.
girNamespace :: GIRXMLNamespace -> Text
girNamespace GLibGIRNS = "http://www.gtk.org/introspection/glib/1.0"
girNamespace CGIRNS = "http://www.gtk.org/introspection/c/1.0"

-- | Lookup an attribute for an element, given the namespace where it lives.
lookupAttrWithNamespace :: GIRXMLNamespace -> Name -> Element -> Maybe Text
lookupAttrWithNamespace ns attr element =
    let attr' = attr {nameNamespace = Just (girNamespace ns)}
    in M.lookup attr' (elementAttributes element)
