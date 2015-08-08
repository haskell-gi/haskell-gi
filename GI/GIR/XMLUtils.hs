-- | Some helpers for making traversals of GIR documents easier.
module GI.GIR.XMLUtils
    ( nodeToElement
    , subelements
    , localName
    , childElemsWithLocalName
    , firstChildWithLocalName
    , getElementContent
    ) where

import Text.XML (Element(elementNodes, elementName),
                 Node(NodeContent, NodeElement), nameLocalName)
import Data.Maybe (mapMaybe, listToMaybe)
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

-- | Find the first child element with the given name.
firstChildWithLocalName :: Text -> Element -> Maybe Element
firstChildWithLocalName n = listToMaybe . childElemsWithLocalName n

-- | Get the content of a given element, if it exists.
getElementContent :: Element -> Maybe Text
getElementContent = listToMaybe . mapMaybe getContent . elementNodes
    where getContent :: Node -> Maybe Text
          getContent (NodeContent t) = Just t
          getContent _ = Nothing

