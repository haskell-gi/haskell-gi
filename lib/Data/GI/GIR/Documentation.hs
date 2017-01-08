-- | Parsing of documentation nodes.
module Data.GI.GIR.Documentation
    ( Documentation(..)
    , queryDocumentation
    ) where

import Data.Text (Text)
import Text.XML (Element)

import Data.GI.GIR.XMLUtils (firstChildWithLocalName, getElementContent,
                             lookupAttr)

-- | Documentation for a given element. The documentation text is
-- typically encoded in the gtk-doc format, see
-- https://developer.gnome.org/gtk-doc-manual/ . This can be parsed
-- with `Data.GI.GIR.parseGtkDoc`.
data Documentation = Documentation { rawDocText   :: Maybe Text
                                   , sinceVersion :: Maybe Text
                                   } deriving (Show, Eq, Ord)

-- | Parse the documentation node for the given element of the GIR file.
queryDocumentation :: Element -> Documentation
queryDocumentation element = Documentation {
  rawDocText = firstChildWithLocalName "doc" element >>= getElementContent,
  sinceVersion = lookupAttr "version" element
  }
