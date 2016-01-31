-- | Parsing of documentation nodes.
module GI.GIR.Documentation
    ( Documentation(..)
    , queryDocumentation
    ) where

import Data.Text (Text)
import Text.XML (Element)

import GI.GIR.XMLUtils (firstChildWithLocalName, getElementContent)

-- | Documentation for a given element.
data Documentation = Documentation {
      docText :: Text
    } deriving (Show, Eq)

-- | Parse the documentation node for the given element of the GIR file.
queryDocumentation :: Element -> Maybe Documentation
queryDocumentation element = fmap Documentation
    (firstChildWithLocalName "doc" element >>= getElementContent)
