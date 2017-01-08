module Data.GI.GIR.Deprecation
    ( DeprecationInfo(..)
    , queryDeprecated
    ) where

import qualified Data.Map as M
import Data.Text (Text)
import Text.XML (Element(elementAttributes))

import Data.GI.GIR.XMLUtils (firstChildWithLocalName, getElementContent)

-- | Deprecation information on a symbol.
data DeprecationInfo = DeprecationInfo {
      deprecatedSinceVersion :: Maybe Text,
      deprecationMessage     :: Maybe Text
    } deriving (Show, Eq)

-- | Parse the deprecation information for the given element of the GIR file.
queryDeprecated :: Element -> Maybe DeprecationInfo
queryDeprecated element =
    case M.lookup "deprecated" attrs of
      Just _ -> let version = M.lookup "deprecated-version" attrs
                    msg = firstChildWithLocalName "doc-deprecated" element >>=
                          getElementContent
                in Just (DeprecationInfo version msg)
      Nothing -> Nothing
    where attrs = elementAttributes element
