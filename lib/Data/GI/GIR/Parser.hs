-- | The Parser monad.
module Data.GI.GIR.Parser
    ( Parser
    , ParseContext(..)
    , ParseError
    , parseError

    , runParser

    , parseName
    , parseDeprecation
    , parseDocumentation
    , parseIntegral
    , parseBool
    , parseChildrenWithLocalName
    , parseAllChildrenWithLocalName
    , parseChildrenWithNSName

    , getAttr
    , getAttrWithNamespace
    , queryAttr
    , queryAttrWithNamespace
    , optionalAttr

    , currentNamespace
    , qualifyName
    , resolveQualifiedTypeName

    -- Reexported for convenience
    , Name(..)
    , Element
    , GIRXMLNamespace(..)
    , DeprecationInfo
    , Documentation
    ) where

import Control.Monad.Except
import Control.Monad.Reader

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Text (Text)
import qualified Text.XML as XML
import Text.XML (Element(elementAttributes))
import Text.Show.Pretty (ppShow)

import Data.GI.GIR.BasicTypes (Name(..), Alias(..), Type(TInterface))
import Data.GI.GIR.Deprecation (DeprecationInfo, queryDeprecated)
import Data.GI.GIR.Documentation (Documentation, queryDocumentation)
import Data.GI.GIR.XMLUtils (localName, GIRXMLNamespace(..),
                        childElemsWithLocalName, childElemsWithNSName,
                        lookupAttr, lookupAttrWithNamespace)

-- | Info to carry around when parsing.
data ParseContext = ParseContext {
      ctxNamespace     :: Text,
      -- Location in the XML tree of the node being parsed (for
      -- debugging purposes).
      treePosition     :: [Text],
      -- Current element being parsed (to be set by withElement)
      currentElement   :: Element,
      knownAliases     :: M.Map Alias Type
    } deriving Show

-- | A message describing a parsing error in human readable form.
type ParseError = Text

-- | Monad where parsers live: we carry a context around, and can
-- throw errors that abort the parsing.
type Parser a = ReaderT ParseContext (Except ParseError) a

-- | Throw a parse error.
parseError :: ParseError -> Parser a
parseError msg = do
  ctx <- ask
  let position = (T.intercalate " / " . reverse . treePosition) ctx
  throwError $ "Error when parsing \"" <> position <> "\": " <> msg <> "\n"
                 <> (T.pack . ppShow . currentElement) ctx

-- | Build a textual description (for debug purposes) of a given element.
elementDescription :: Element -> Text
elementDescription element =
    case M.lookup "name" (elementAttributes element) of
      Nothing -> localName element
      Just n -> localName element <> " [" <> n <> "]"

-- | Build a name in the current namespace.
nameInCurrentNS :: Text -> Parser Name
nameInCurrentNS n = do
  ctx <- ask
  return $ Name (ctxNamespace ctx) n

-- | Return the current namespace.
currentNamespace :: Parser Text
currentNamespace = ctxNamespace <$> ask

-- | Check whether there is an alias for the given name, and return
-- the corresponding type in case it exists, and otherwise a TInterface.
resolveQualifiedTypeName :: Name -> Parser Type
resolveQualifiedTypeName name = do
  ctx <- ask
  case M.lookup (Alias name) (knownAliases ctx) of
    -- The resolved type may be an alias itself, like for
    -- Gtk.Allocation -> Gdk.Rectangle -> cairo.RectangleInt
    Just (TInterface n) -> resolveQualifiedTypeName n
    Just t -> return t
    Nothing -> return $ TInterface name

-- | Return the value of an attribute for the given element. If the
-- attribute is not present this throws an error.
getAttr :: XML.Name -> Parser Text
getAttr attr = do
  ctx <- ask
  case lookupAttr attr (currentElement ctx) of
    Just val -> return val
    Nothing -> parseError $ "Expected attribute \"" <>
               (T.pack . show) attr <> "\" not present."

-- | Like 'getAttr', but allow for specifying the namespace.
getAttrWithNamespace :: GIRXMLNamespace -> XML.Name -> Parser Text
getAttrWithNamespace ns attr = do
  ctx <- ask
  case lookupAttrWithNamespace ns attr (currentElement ctx) of
    Just val -> return val
    Nothing -> parseError $ "Expected attribute \"" <>
               (T.pack . show) attr <> "\" in namespace \"" <>
               (T.pack . show) ns <> "\" not present."

-- | Return the value of an attribute if it is present, and Nothing otherwise.
queryAttr :: XML.Name -> Parser (Maybe Text)
queryAttr attr = do
  ctx <- ask
  return $ lookupAttr attr (currentElement ctx)

-- | Like `queryAttr`, but allow for specifying the namespace.
queryAttrWithNamespace :: GIRXMLNamespace -> XML.Name -> Parser (Maybe Text)
queryAttrWithNamespace ns attr = do
  ctx <- ask
  return $ lookupAttrWithNamespace ns attr (currentElement ctx)

-- | Ask for an optional attribute, applying the given parser to
-- it. If the argument does not exist return the default value provided.
optionalAttr :: XML.Name -> a -> (Text -> Parser a) -> Parser a
optionalAttr attr def parser =
    queryAttr attr >>= \case
              Just a -> parser a
              Nothing -> return def

-- | Build a 'Name' out of the (possibly qualified) supplied name. If
-- the supplied name is unqualified we qualify with the current
-- namespace, and otherwise we simply parse it.
qualifyName :: Text -> Parser Name
qualifyName n = case T.split (== '.') n of
    [ns, name] -> return $ Name ns name
    [name] -> nameInCurrentNS name
    _ -> parseError "Could not understand name"

-- | Get the qualified name for the current element.
parseName :: Parser Name
parseName = getAttr "name" >>= qualifyName

-- | Parse the deprecation text, if present.
parseDeprecation :: Parser (Maybe DeprecationInfo)
parseDeprecation = do
  ctx <- ask
  return $ queryDeprecated (currentElement ctx)

-- | Parse the documentation info for the current node.
parseDocumentation :: Parser Documentation
parseDocumentation = do
  ctx <- ask
  return $ queryDocumentation (currentElement ctx)

-- | Parse a signed integral number.
parseIntegral :: Integral a => Text -> Parser a
parseIntegral str =
    case TR.signed TR.decimal str of
      Right (n, r) | T.null r -> return n
      _ -> parseError $ "Could not parse integral value: \"" <> str <> "\"."

-- | A boolean value given by a numerical constant.
parseBool :: Text -> Parser Bool
parseBool "0" = return False
parseBool "1" = return True
parseBool other = parseError $ "Unsupported boolean value: " <> T.pack (show other)

-- | Parse all the introspectable subelements with the given local name.
parseChildrenWithLocalName :: Text -> Parser a -> Parser [a]
parseChildrenWithLocalName n parser = do
  ctx <- ask
  let introspectableChildren = filter introspectable
                               (childElemsWithLocalName n (currentElement ctx))
  mapM (withElement parser) introspectableChildren
      where introspectable :: Element -> Bool
            introspectable e = lookupAttr "introspectable" e /= Just "0" &&
                               lookupAttr "shadowed-by" e == Nothing

-- | Parse all subelements with the given local name.
parseAllChildrenWithLocalName :: Text -> Parser a -> Parser [a]
parseAllChildrenWithLocalName n parser = do
  ctx <- ask
  mapM (withElement parser) (childElemsWithLocalName n (currentElement ctx))

-- | Parse all introspectable children with the given namespace and
-- local name.
parseChildrenWithNSName :: GIRXMLNamespace -> Text -> Parser a -> Parser [a]
parseChildrenWithNSName ns n parser = do
  ctx <- ask
  let introspectableChildren = filter introspectable
                               (childElemsWithNSName ns n (currentElement ctx))
  mapM (withElement parser) introspectableChildren
      where introspectable :: Element -> Bool
            introspectable e = lookupAttr "introspectable" e /= Just "0"

-- | Run the given parser for a given subelement in the XML tree.
withElement :: Parser a -> Element -> Parser a
withElement parser element = local modifyParsePosition parser
    where modifyParsePosition ctx =
              ctx { treePosition = elementDescription element : treePosition ctx
                  , currentElement = element}

-- | Run the given parser, returning either success or an error.
runParser :: Text -> M.Map Alias Type -> Element -> Parser a ->
             Either ParseError a
runParser ns aliases element parser =
    runExcept (runReaderT parser ctx)
              where ctx = ParseContext {
                            ctxNamespace = ns
                          , treePosition = [elementDescription element]
                          , currentElement = element
                          , knownAliases = aliases
                          }
