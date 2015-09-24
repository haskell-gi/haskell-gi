{-# LANGUAGE OverloadedStrings #-}
module GI.GIR.Deprecation
    ( DeprecationInfo
    , deprecatedPragma
    , queryDeprecated
    ) where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Text.XML (Element(elementAttributes))

import GI.GIR.XMLUtils (firstChildWithLocalName, getElementContent)

-- | Deprecation information on a symbol.
data DeprecationInfo = DeprecationInfo {
      deprecatedSinceVersion :: Maybe Text,
      deprecationMessage     :: Maybe Text
    } deriving (Show, Eq)

-- | Encode the given `DeprecationInfo` for the given symbol as a
-- deprecation pragma.
deprecatedPragma :: String -> Maybe DeprecationInfo -> String
deprecatedPragma _    Nothing     = ""
deprecatedPragma name (Just info) = "{-# DEPRECATED " ++ name ++ reason ++ note ++ "#-}"
        where reason = case deprecationMessage info of
                         Nothing -> " <no reason given for deprecation> "
                         Just msg -> " \"" ++ T.unpack msg ++ "\" "
              note = case deprecatedSinceVersion info of
                       Nothing -> ""
                       Just v -> "(since version " ++ T.unpack v ++ " ) "

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
