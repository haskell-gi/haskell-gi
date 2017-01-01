-- | Parsing of documentation nodes.
module Data.GI.GIR.Documentation
    ( Documentation(..)
    , queryDocumentation
    ) where

import Data.Text (Text)
import Text.XML (Element)

import Data.GI.GIR.XMLUtils (firstChildWithLocalName, getElementContent)

-- | Documentation for a given element. The documentation text is
-- typically encoded in the gtk-doc format, see
-- https://developer.gnome.org/gtk-doc-manual/ . This can be parsed
-- with `Data.GI.GIR.parseGtkDoc`.
data Documentation = Documentation {
      rawDocText :: Text
    } deriving (Show, Eq)

-- | Parse the documentation node for the given element of the GIR file.
queryDocumentation :: Element -> Maybe Documentation
queryDocumentation element = fmap Documentation
    (firstChildWithLocalName "doc" element >>= getElementContent)
