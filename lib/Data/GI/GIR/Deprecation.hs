module Data.GI.GIR.Deprecation
    ( DeprecationInfo
    , deprecatedPragma
    , queryDeprecated
    ) where

import Data.Monoid ((<>))
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Text.XML (Element(elementAttributes))

import Data.GI.GIR.XMLUtils (firstChildWithLocalName, getElementContent)

-- | Deprecation information on a symbol.
data DeprecationInfo = DeprecationInfo {
      deprecatedSinceVersion :: Maybe Text,
      deprecationMessage     :: Maybe Text
    } deriving (Show, Eq)

-- | Encode the given `DeprecationInfo` for the given symbol as a
-- deprecation pragma.
deprecatedPragma :: Text -> Maybe DeprecationInfo -> Text
deprecatedPragma _    Nothing     = ""
deprecatedPragma name (Just info) = "{-# DEPRECATED " <> name <> " " <>
                                    (T.pack . show) (note <> reason) <> "#-}"
        where reason = case deprecationMessage info of
                         Nothing -> []
                         Just msg -> T.lines msg
              note = case deprecatedSinceVersion info of
                       Nothing -> []
                       Just v -> ["(Since version " <> v <> ")"]

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
