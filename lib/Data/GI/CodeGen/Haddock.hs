-- | Render an abstract representation of documentation (as produced
-- by `parseGtkDoc`) as Haddock formatted documentation.
module Data.GI.CodeGen.Haddock
  ( deprecatedPragma
  , writeDocumentation
  , RelativeDocPosition(..)
  , writeHaddock
  , writeArgDocumentation
  , writeReturnDocumentation
  , addSectionDocumentation
  ) where

#if !MIN_VERSION_base(4,13,0)
import Control.Monad (mapM_, unless)
#else
import Control.Monad (unless)
#endif
import qualified Data.Map as M
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import qualified Data.Text as T
import Data.Text (Text)

import Data.GI.GIR.Arg (Arg(..))
import Data.GI.GIR.Callable (Callable(..))
import Data.GI.GIR.Deprecation (DeprecationInfo(..))
import Data.GI.GIR.Documentation (Documentation(..))

import Data.GI.CodeGen.Code (CodeGen, config, line, HaddockSection,
                             getC2HMap, addSectionFormattedDocs)
import Data.GI.CodeGen.Config (modName, overrides)
import Data.GI.CodeGen.CtoHaskellMap (Hyperlink(..))
import Data.GI.CodeGen.GtkDoc (GtkDoc(..), Token(..), CRef(..), Language(..),
                               Link(..), ListItem(..), parseGtkDoc)
import Data.GI.CodeGen.Overrides (onlineDocsMap)
import Data.GI.CodeGen.SymbolNaming (lowerSymbol, signalHaskellName)

-- | Where is the documentation located with respect to the relevant
-- symbol, useful for determining whether we want to start with @|@ or @^@.
data RelativeDocPosition = DocBeforeSymbol
                         | DocAfterSymbol

-- | Given a `GtkDoc`, a map from C identifiers to Haskell symbols,
-- and a location online where to find the C documentation, render the
-- corresponding Haddock-formatted text. Note that the comment
-- delimiters are not included in the output.
--
-- === __Examples__
-- >>> formatHaddock M.empty "" (GtkDoc [Literal "Hello ", Literal "World!"])
-- "Hello World!"
--
-- >>> let c2h = M.fromList [(FunctionRef "foo", ValueIdentifier "foo")]
-- >>> formatHaddock c2h "" (GtkDoc [SymbolRef (FunctionRef "foo")])
-- "'foo'"
--
-- >>> let onlineDocs = "http://wiki.haskell.org"
-- >>> formatHaddock M.empty onlineDocs (GtkDoc [ExternalLink (Link "GI" "GObjectIntrospection")])
-- "<http://wiki.haskell.org/GObjectIntrospection GI>"
--
-- >>> formatHaddock M.empty "a" (GtkDoc [List [ListItem (GtkDoc [Image (Link "test" "test.png")]) []]])
-- "\n* <<a/test.png test>>\n"
formatHaddock :: M.Map CRef Hyperlink -> Text -> GtkDoc -> Text
formatHaddock c2h docBase (GtkDoc doc) = T.concat $ map formatToken doc
  where formatToken :: Token -> Text
        formatToken (Literal l) = escape l
        formatToken (Comment _) = ""
        formatToken (Verbatim v) = "@" <> escape v <> "@"
        formatToken (CodeBlock l c) = formatCodeBlock l c
        formatToken (ExternalLink l) = formatLink l docBase
        formatToken (Image l) = formatImage l docBase
        formatToken (SectionHeader l h) = formatSectionHeader c2h docBase l h
        formatToken (List l) = formatList c2h docBase l
        formatToken (SymbolRef cr) = case M.lookup cr c2h of
          Just hr -> formatHyperlink hr
          Nothing -> formatUnknownCRef c2h cr

-- | Format a `CRef` whose Haskell representation is not known.
formatUnknownCRef :: M.Map CRef Hyperlink -> CRef -> Text
formatUnknownCRef _ (FunctionRef f) = formatCRef $ f <> "()"
formatUnknownCRef _ (ParamRef p) = "/@" <> lowerSymbol p <> "@/"
formatUnknownCRef _ (LocalSignalRef s) =
  let sn = signalHaskellName s
  in "[" <> sn <> "](#g:signal:" <> sn <> ")"
formatUnknownCRef c2h (SignalRef owner signal) =
  case M.lookup (TypeRef owner) c2h of
    Nothing -> formatCRef $ owner <> "::" <> signal
    Just r -> formatHyperlink r <> "::" <> formatCRef signal
formatUnknownCRef c2h (PropertyRef owner prop) =
  case M.lookup (TypeRef owner) c2h of
    Nothing -> formatCRef $ owner <> ":" <> prop
    Just r -> formatHyperlink r <> ":" <> formatCRef prop
formatUnknownCRef c2h (VMethodRef owner vmethod) =
  case M.lookup (TypeRef owner) c2h of
    Nothing -> formatCRef $ owner <> "." <> vmethod <> "()"
    Just r -> formatHyperlink r <> "." <> formatCRef vmethod <> "()"
formatUnknownCRef c2h (StructFieldRef owner field) =
  case M.lookup (TypeRef owner) c2h of
    Nothing -> formatCRef $ owner <> "." <> field
    Just r -> formatHyperlink r <> "." <> formatCRef field
formatUnknownCRef _ (TypeRef t) = formatCRef t
formatUnknownCRef _ (ConstantRef t) = formatCRef t

-- | Formatting for an unknown C reference.
formatCRef :: Text -> Text
formatCRef t = "@/" <> escape t <> "/@"

-- | Format a `Hyperlink` into plain `Text`.
formatHyperlink :: Hyperlink -> Text
formatHyperlink (TypeIdentifier t) = "t'" <> t <> "'"
formatHyperlink (ValueIdentifier t) = "'" <> t <> "'"
formatHyperlink (ModuleLink m) = "\"" <> m <> "\""
formatHyperlink (ModuleLinkWithAnchor mLabel m a) =
  case mLabel of
    Nothing -> "\"" <> m <> "#" <> a <> "\""
    Just label -> "[" <> label <> "](\"" <> m <> "#" <> a <> "\")"

-- | Format a code block in a specified language.
formatCodeBlock :: Maybe Language -> Text -> Text
formatCodeBlock maybeLang code =
  let header = case maybeLang of
        Nothing -> ""
        Just (Language lang) -> "\n=== /" <> lang <> " code/\n"
      birdTrack = T.unlines . map (T.cons '>') . T.lines
  in header <> birdTrack code

-- | Qualify the given address with the docBase, if it is not an
-- absolute address.
qualifiedWith :: Text -> Text -> Text
qualifiedWith address docBase =
  if "http://" `T.isPrefixOf` address || "https://" `T.isPrefixOf` address
  then address
  else if "/" `T.isSuffixOf` docBase
       then docBase <> address
       else docBase <> "/" <> address

-- | Format a link to some external resource.
formatLink :: Link -> Text -> Text
formatLink (Link {linkName = name, linkAddress = address}) docBase =
  let address' = address `qualifiedWith` docBase
      name' = T.replace ">" "\\>" name
  in "<" <> address' <> " " <>  name' <> ">"

-- | Format an embedded image.
formatImage :: Link -> Text -> Text
formatImage (Link {linkName = name, linkAddress = address}) docBase =
  let address' = address `qualifiedWith` docBase
      name' = T.replace ">" "\\>" name
  in if T.null name'
     then "<<" <> address' <> ">>"
     else "<<" <> address' <> " " <>  name' <> ">>"

-- | Format a section header of the given level and with the given
-- text. Note that the level will be truncated to 2, if it is larger
-- than that.
formatSectionHeader :: M.Map CRef Hyperlink -> Text -> Int -> GtkDoc -> Text
formatSectionHeader c2h docBase level header =
  T.replicate level "=" <> " " <> formatHaddock c2h docBase header <> "\n"

-- | Format a list of items.
formatList :: M.Map CRef Hyperlink -> Text -> [ListItem] -> Text
formatList c2h docBase items = "\n" <> T.concat (map formatListItem items)
  where formatListItem :: ListItem -> Text
        formatListItem (ListItem first rest) =
          "* " <> format first <> "\n"
          <> T.concat (map ((<> "\n") . format) rest)

        format :: GtkDoc -> Text
        format = formatHaddock c2h docBase

-- | Escape the reserved Haddock characters in a given `Text`.
--
-- === __Examples__
-- >>> escape "\""
-- "\\\""
--
-- >>> escape "foo@bar.com"
-- "foo\\@bar.com"
--
-- >>> escape "C:\\Applications"
-- "C:\\\\Applications"
escape :: Text -> Text
escape = T.concatMap escapeChar
  where
    escapeChar :: Char -> Text
    escapeChar c = if c `elem` ("\\/'`\"@<" :: [Char])
                   then "\\" <> T.singleton c
                   else T.singleton c

-- | Get the base url for the online C language documentation for the
-- module being currently generated.
getDocBase :: CodeGen Text
getDocBase = do
  mod <- modName <$> config
  docsMap <- (onlineDocsMap . overrides) <$> config
  return $ case M.lookup mod docsMap of
             Just url -> url
             Nothing -> "http://developer.gnome.org/" <> T.toLower mod <>
                        "/stable"

-- | Write the deprecation pragma for the given `DeprecationInfo`, if
-- not `Nothing`.
deprecatedPragma :: Text -> Maybe DeprecationInfo -> CodeGen ()
deprecatedPragma _  Nothing = return ()
deprecatedPragma name (Just info) = do
  c2h <- getC2HMap
  docBase <- getDocBase
  line $ "{-# DEPRECATED " <> name <> " " <>
    (T.pack . show) (note <> reason c2h docBase) <> " #-}"
        where reason c2h docBase =
                case deprecationMessage info of
                  Nothing -> []
                  Just msg -> map (formatHaddock c2h docBase . parseGtkDoc)
                                  (T.lines msg)
              note = case deprecatedSinceVersion info of
                       Nothing -> []
                       Just v -> ["(Since version " <> v <> ")"]

-- | Format the given documentation into a set of lines. Note that
-- this does include the opening or ending comment delimiters.
formatDocumentation :: M.Map CRef Hyperlink -> Text -> Documentation -> Text
formatDocumentation c2h docBase doc = do
  let description = case rawDocText doc of
        Just raw -> formatHaddock c2h docBase (parseGtkDoc raw)
        Nothing -> "/No description available in the introspection data./"
  description <> case sinceVersion doc of
                   Nothing -> ""
                   Just ver -> "\n\n/Since: " <> ver <> "/"

-- | Write the given documentation into generated code.
writeDocumentation :: RelativeDocPosition -> Documentation -> CodeGen ()
writeDocumentation pos doc = do
  c2h <- getC2HMap
  docBase <- getDocBase
  writeHaddock pos (formatDocumentation c2h docBase doc)

-- | Like `writeDocumentation`, but allows us to pass explicitly the
-- Haddock comment to write.
writeHaddock :: RelativeDocPosition -> Text -> CodeGen ()
writeHaddock pos haddock =
  let marker = case pos of
        DocBeforeSymbol -> "|"
        DocAfterSymbol -> "^"
      lines = case T.lines haddock of
        [] -> []
        (first:rest) -> ("-- " <> marker <> " " <> first) : map ("-- " <>) rest
  in mapM_ line lines

-- | Write the documentation for the given argument.
writeArgDocumentation :: Arg -> CodeGen ()
writeArgDocumentation arg =
  case rawDocText (argDoc arg) of
    Nothing -> return ()
    Just raw -> do
      c2h <- getC2HMap
      docBase <- getDocBase
      let haddock = "/@" <> lowerSymbol (argCName arg) <> "@/: " <>
                    formatHaddock c2h docBase (parseGtkDoc raw)
      writeHaddock DocAfterSymbol haddock

-- | Write the documentation for the given return value.
writeReturnDocumentation :: Callable -> Bool -> CodeGen ()
writeReturnDocumentation callable skip = do
  c2h <- getC2HMap
  docBase <- getDocBase
  let returnValInfo = if skip
                      then []
                      else case rawDocText (returnDocumentation callable) of
                             Nothing -> []
                             Just raw -> ["__Returns:__ " <>
                                           formatHaddock c2h docBase
                                           (parseGtkDoc raw)]
      throwsInfo = if callableThrows callable
                   then ["/(Can throw 'Data.GI.Base.GError.GError')/"]
                   else []
  let fullInfo = T.intercalate " " (returnValInfo ++ throwsInfo)
  unless (T.null fullInfo) $
    writeHaddock DocAfterSymbol fullInfo

-- | Add the given text to the documentation for the section being generated.
addSectionDocumentation :: HaddockSection -> Documentation -> CodeGen ()
addSectionDocumentation section doc = do
  c2h <- getC2HMap
  docBase <- getDocBase
  let formatted = formatDocumentation c2h docBase doc
  addSectionFormattedDocs section formatted
