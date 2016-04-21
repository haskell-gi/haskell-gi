module Data.GI.GIR.Alias
    ( documentListAliases
    ) where

import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Text.XML (Element(elementAttributes), Document(documentRoot))

import Data.GI.GIR.BasicTypes (Alias(..), Type)
import Data.GI.GIR.Type (parseType)
import Data.GI.GIR.Parser
import Data.GI.GIR.XMLUtils (childElemsWithLocalName)

-- | Find all aliases in a given namespace.
namespaceListAliases :: Element -> M.Map Alias Type
namespaceListAliases ns =
    case M.lookup "name" (elementAttributes ns) of
      Nothing -> error $ "Namespace with no name!"
      Just nsName -> case runParser nsName M.empty ns parseAliases of
                       Left err -> (error . T.unpack) err
                       Right aliases -> M.fromList (map addNS aliases)
                           where addNS (n, t) = (Alias (nsName, n), t)

-- | Parse all the aliases in the current namespace
parseAliases :: Parser [(Text, Type)]
parseAliases = parseChildrenWithLocalName "alias" parseAlias

-- | Parse a single alias
parseAlias :: Parser (Text, Type)
parseAlias = do
  name <- getAttr "name"
  t <- parseType
  return (name, t)

-- | Find all aliases in a given document.
documentListAliases :: Document -> M.Map Alias Type
documentListAliases doc = M.unions (map namespaceListAliases namespaces)
    where namespaces = childElemsWithLocalName "namespace" (documentRoot doc)
