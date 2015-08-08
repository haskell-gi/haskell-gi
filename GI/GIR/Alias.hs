{-# LANGUAGE OverloadedStrings #-}
module GI.GIR.Alias
    ( documentListAliases
    ) where

import qualified Data.Map as M
import Data.Text (Text)
import Text.XML (Element(elementAttributes), Document(documentRoot))

import GI.Type (Type)
import GI.GIR.BasicTypes (Alias(..), ParseContext(..))
import GI.GIR.Type (parseType)
import GI.GIR.XMLUtils (subelements, childElemsWithLocalName, localName)

-- | Find all aliases in a given namespace. We assume that namespaces
-- are the first elements in the namespace, in order to avoid scanning
-- all the entries in the namespace.
namespaceListAliases :: Element -> M.Map Alias Type
namespaceListAliases ns = M.fromList $ maybe [] (go (subelements ns)) maybeNSName
    where maybeNSName = M.lookup "name" (elementAttributes ns)
          go :: [Element] -> Text -> [(Alias, Type)]
          go [] _ = []
          go (e:es) nsName
             | localName e /= "alias" = []
             | otherwise = case parseAlias nsName e of
                             Just n -> n : go es nsName
                             Nothing -> go es nsName

-- | Parse an alias
parseAlias :: Text -> Element -> Maybe (Alias, Type)
parseAlias nsName element = do
  name <- M.lookup "name" (elementAttributes element)
  t <- parseType ctx element
  return (Alias (nsName, name), t)
  where ctx = ParseContext {currentNamespace = nsName,
                            knownAliases = M.empty}

-- | Find all aliases in a given document.
documentListAliases :: Document -> M.Map Alias Type
documentListAliases doc = M.unions (map namespaceListAliases namespaces)
    where namespaces = childElemsWithLocalName "namespace" (documentRoot doc)
