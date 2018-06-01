-- | Some helpers for making traversals of GIR documents easier.
module Data.GI.GIR.XMLUtils
    ( nodeToElement
    , subelements
    , localName
    , lookupAttr
    , GIRXMLNamespace(..)
    , lookupAttrWithNamespace
    , childElemsWithLocalName
    , childElemsWithNSName
    , firstChildWithLocalName
    , getElementContent
    , xmlLocalName
    , xmlNSName
    ) where

import Text.XML (Element(elementNodes, elementName, elementAttributes),
                 Node(NodeContent, NodeElement), nameLocalName, Name(..))
import Data.Maybe (mapMaybe, listToMaybe)
import qualified Data.Map as M
import Data.Text (Text)

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

-- | Restrict to those with given name.
childElemsWithNSName :: GIRXMLNamespace -> Text -> Element -> [Element]
childElemsWithNSName ns n = filter nameMatch . subelements
    where nameMatch = (== name) . elementName
          name = Name {
                   nameLocalName = n
                 , nameNamespace = Just (girNamespace ns)
                 , namePrefix = Nothing
                 }

-- | Find the first child element with the given name.
firstChildWithLocalName :: Text -> Element -> Maybe Element
firstChildWithLocalName n = listToMaybe . childElemsWithLocalName n

-- | Get the content of a given element, if it exists.
getElementContent :: Element -> Maybe Text
getElementContent = listToMaybe . mapMaybe getContent . elementNodes
    where getContent :: Node -> Maybe Text
          getContent (NodeContent t) = Just t
          getContent _ = Nothing

-- | Lookup an attribute for an element (with no prefix).
lookupAttr :: Name -> Element -> Maybe Text
lookupAttr attr element = M.lookup attr (elementAttributes element)

-- | GIR namespaces we know about.
data GIRXMLNamespace = GLibGIRNS | CGIRNS | CoreGIRNS
                     deriving Show

-- | Return the text representation of the known GIR namespaces.
girNamespace :: GIRXMLNamespace -> Text
girNamespace GLibGIRNS = "http://www.gtk.org/introspection/glib/1.0"
girNamespace CGIRNS = "http://www.gtk.org/introspection/c/1.0"
girNamespace CoreGIRNS = "http://www.gtk.org/introspection/core/1.0"

-- | Lookup an attribute for an element, given the namespace where it lives.
lookupAttrWithNamespace :: GIRXMLNamespace -> Name -> Element -> Maybe Text
lookupAttrWithNamespace ns attr element =
    let attr' = attr {nameNamespace = Just (girNamespace ns)}
    in M.lookup attr' (elementAttributes element)


-- | Construct a `Text.XML.Name` by only giving the local name.
xmlLocalName :: Text -> Name
xmlLocalName n = Name { nameLocalName = n
                      , nameNamespace = Nothing
                      , namePrefix = Nothing }

-- | Construct a `Text.XML.Name` specifying a namespace too.
xmlNSName :: GIRXMLNamespace -> Text -> Name
xmlNSName ns n = Name { nameLocalName = n
                      , nameNamespace = Just (girNamespace ns)
                      , namePrefix = Nothing }
