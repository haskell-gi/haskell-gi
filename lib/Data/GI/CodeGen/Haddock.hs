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
import Data.GI.GIR.BasicTypes (Name(Name))
import Data.GI.GIR.Callable (Callable(..))
import Data.GI.GIR.Deprecation (DeprecationInfo(..))
import Data.GI.GIR.Documentation (Documentation(..))

import Data.GI.CodeGen.Code (CodeGen, config, line, HaddockSection,
                             getC2HMap, addSectionFormattedDocs)
import Data.GI.CodeGen.Config (modName, overrides)
import Data.GI.CodeGen.CtoHaskellMap (Hyperlink(..))
import Data.GI.CodeGen.GtkDoc (GtkDoc(..), Token(..), CRef(..), Language(..),
                               Link(..), ListItem(..), parseGtkDoc,
                               DocSymbolName(..), resolveDocSymbol, docName)
import Data.GI.CodeGen.Overrides (onlineDocsMap)
import Data.GI.CodeGen.SymbolNaming (lowerSymbol, signalHaskellName,
                                     haddockSignalAnchor)

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
-- >>> formatHaddock M.empty "" "Test" (GtkDoc [Literal "Hello ", Literal "World!"])
-- "Hello World!"
--
-- >>> let c2h = M.fromList [(OldFunctionRef "foo", ValueIdentifier "foo")]
-- >>> formatHaddock c2h "" "Test" (GtkDoc [SymbolRef (OldFunctionRef "foo")])
-- "'foo'"
--
-- >>> let onlineDocs = "http://wiki.haskell.org"
-- >>> formatHaddock M.empty onlineDocs "Test" (GtkDoc [ExternalLink (Link "GI" "GObjectIntrospection")])
-- "<http://wiki.haskell.org/GObjectIntrospection GI>"
--
-- >>> formatHaddock M.empty "a" "Test" (GtkDoc [List [ListItem (GtkDoc [Image (Link "test" "test.png")]) []]])
-- "\n* <<a/test.png test>>\n"
formatHaddock :: M.Map CRef Hyperlink -> Text -> Text -> GtkDoc -> Text
formatHaddock c2h docBase defaultNS (GtkDoc tokens) = T.concat $ map formatToken tokens
  where formatToken :: Token -> Text
        formatToken (Literal l) = escape l
        formatToken (Comment _) = ""
        formatToken (Verbatim v) = "@" <> escape v <> "@"
        formatToken (CodeBlock l c) = formatCodeBlock l c
        formatToken (ExternalLink l) = formatLink l docBase
        formatToken (Image l) = formatImage l docBase
        formatToken (SectionHeader l h) =
          formatSectionHeader c2h docBase defaultNS l h
        formatToken (List l) = formatList c2h docBase defaultNS l
        formatToken (SymbolRef cr) = case M.lookup cr c2h of
          Just hr -> formatHyperlink hr
          Nothing -> formatUnknownCRef c2h defaultNS cr

-- | Format a `CRef` whose Haskell representation is not known, using
-- a provided default namespace for relative symbols.
formatUnknownCRef :: M.Map CRef Hyperlink -> Text -> CRef -> Text
formatUnknownCRef _ _ (OldFunctionRef f) = formatCRef $ f <> "()"
formatUnknownCRef _ defaultNS (FunctionRef n) =
  formatCRef $ formatDocSymbol n defaultNS
formatUnknownCRef _ _ (ParamRef p) = "/@" <> lowerSymbol p <> "@/"
formatUnknownCRef _ _ (LocalSignalRef s) =
  let sn = signalHaskellName s
  in "[" <> sn <> "](#" <> haddockSignalAnchor <> sn <> ")"
formatUnknownCRef c2h defaultNS (SignalRef docSymbol signal) =
  let owner@(Name ns n) = resolveDocSymbol docSymbol defaultNS
  in case M.lookup (TypeRef (docName owner)) c2h of
    Nothing -> formatCRef $ ns <> "." <> n <> "::" <> signal
    Just r -> formatHyperlink r <> "::" <> formatCRef signal
formatUnknownCRef c2h _ (OldSignalRef owner signal) =
  case M.lookup (CTypeRef owner) c2h of
    Nothing -> formatCRef $ owner <> "::" <> signal
    Just r -> formatHyperlink r <> "::" <> formatCRef signal
formatUnknownCRef c2h _ (OldPropertyRef owner prop) =
  case M.lookup (CTypeRef owner) c2h of
    Nothing -> formatCRef $ owner <> ":" <> prop
    Just r -> formatHyperlink r <> ":" <> formatCRef prop
formatUnknownCRef c2h defaultNS (PropertyRef docSymbol prop) =
  let owner@(Name ns n) = resolveDocSymbol docSymbol defaultNS
  in case M.lookup (TypeRef (docName owner)) c2h of
    Nothing -> formatCRef $ ns <> "." <> n <> ":" <> prop
    Just r -> formatHyperlink r <> ":" <> formatCRef prop
formatUnknownCRef c2h _ (VMethodRef owner vmethod) =
  case M.lookup (CTypeRef owner) c2h of
    Nothing -> formatCRef $ owner <> "." <> vmethod <> "()"
    Just r -> formatHyperlink r <> "." <> formatCRef vmethod <> "()"
formatUnknownCRef c2h defaultNS (VFuncRef docSymbol vmethod) =
  let owner@(Name ns n) = resolveDocSymbol docSymbol defaultNS
  in case M.lookup (TypeRef (docName owner)) c2h of
    Nothing -> formatCRef $ ns <> "." <> n <> "." <> vmethod <> "()"
    Just r -> formatHyperlink r <> "." <> formatCRef vmethod <> "()"
formatUnknownCRef c2h defaultNS(MethodRef docSymbol method) =
  let owner@(Name ns n) = resolveDocSymbol docSymbol defaultNS
  in case M.lookup (TypeRef (docName owner)) c2h of
    Nothing -> formatCRef $ ns <> "." <> n <> "." <> method <> "()"
    Just r -> formatHyperlink r <> "." <> formatCRef method <> "()"
formatUnknownCRef c2h _ (StructFieldRef owner field) =
  case M.lookup (CTypeRef owner) c2h of
    Nothing -> formatCRef $ owner <> "." <> field
    Just r -> formatHyperlink r <> "." <> formatCRef field
formatUnknownCRef _ _ (CTypeRef t) = formatCRef t
formatUnknownCRef _ defaultNS (TypeRef n) =
  formatCRef $ formatDocSymbol n defaultNS
formatUnknownCRef _ _ (ConstantRef t) = formatCRef t

-- | Format the given symbol name in a fully qualified way, using the
-- default namespace if needed.
formatDocSymbol :: DocSymbolName -> Text -> Text
formatDocSymbol (RelativeName n) defaultNS = defaultNS <> "." <> n
formatDocSymbol (AbsoluteName ns n) _ = ns <> "." <> n

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
formatSectionHeader :: M.Map CRef Hyperlink -> Text -> Text -> Int -> GtkDoc -> Text
formatSectionHeader c2h docBase defaultNS level header =
  T.replicate level "=" <> " " <> formatHaddock c2h docBase defaultNS header <> "\n"

-- | Format a list of items.
formatList :: M.Map CRef Hyperlink -> Text -> Text -> [ListItem] -> Text
formatList c2h docBase defaultNS items = "\n" <> T.concat (map formatListItem items)
  where formatListItem :: ListItem -> Text
        formatListItem (ListItem first rest) =
          "* " <> format first <> "\n"
          <> T.concat (map ((<> "\n") . format) rest)

        format :: GtkDoc -> Text
        format doc = formatHaddock c2h docBase defaultNS doc

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
getDocBase :: CodeGen e Text
getDocBase = do
  mod <- modName <$> config
  docsMap <- (onlineDocsMap . overrides) <$> config
  return $ case M.lookup mod docsMap of
             Just url -> url
             Nothing -> "http://developer.gnome.org/" <> T.toLower mod <>
                        "/stable"

-- | Write the deprecation pragma for the given `DeprecationInfo`, if
-- not `Nothing`.
deprecatedPragma :: Text -> Maybe DeprecationInfo -> CodeGen e ()
deprecatedPragma _  Nothing = return ()
deprecatedPragma name (Just info) = do
  c2h <- getC2HMap
  docBase <- getDocBase
  defaultNS <- modName <$> config
  line $ "{-# DEPRECATED " <> name <> " " <>
    (T.pack . show) (note <> reason c2h docBase defaultNS) <> " #-}"
        where reason c2h docBase defaultNS =
                case deprecationMessage info of
                  Nothing -> []
                  Just msg -> map (formatHaddock c2h docBase defaultNS
                                   . parseGtkDoc)
                                  (T.lines msg)
              note = case deprecatedSinceVersion info of
                       Nothing -> []
                       Just v -> ["(Since version " <> v <> ")"]

-- | Format the given documentation into a set of lines. Note that
-- this does include the opening or ending comment delimiters.
formatDocumentation :: M.Map CRef Hyperlink -> Text -> Text -> Documentation -> Text
formatDocumentation c2h docBase defaultNS doc = do
  let description = case rawDocText doc of
        Just raw -> formatHaddock c2h docBase defaultNS (parseGtkDoc raw)
        Nothing -> "/No description available in the introspection data./"
  description <> case sinceVersion doc of
                   Nothing -> ""
                   Just ver -> "\n\n/Since: " <> ver <> "/"

-- | Write the given documentation into generated code.
writeDocumentation :: RelativeDocPosition -> Documentation -> CodeGen e ()
writeDocumentation pos doc = do
  c2h <- getC2HMap
  docBase <- getDocBase
  defaultNS <- modName <$> config
  writeHaddock pos (formatDocumentation c2h docBase defaultNS doc)

-- | Like `writeDocumentation`, but allows us to pass explicitly the
-- Haddock comment to write.
writeHaddock :: RelativeDocPosition -> Text -> CodeGen e ()
writeHaddock pos haddock =
  let marker = case pos of
        DocBeforeSymbol -> "|"
        DocAfterSymbol -> "^"
      lines = case T.lines haddock of
        [] -> []
        (first:rest) -> ("-- " <> marker <> " " <> first) : map ("-- " <>) rest
  in mapM_ line lines

-- | Write the documentation for the given argument.
writeArgDocumentation :: Arg -> CodeGen e ()
writeArgDocumentation arg =
  case rawDocText (argDoc arg) of
    Nothing -> return ()
    Just raw -> do
      c2h <- getC2HMap
      docBase <- getDocBase
      defaultNS <- modName <$> config
      let haddock = "/@" <> lowerSymbol (argCName arg) <> "@/: " <>
                    formatHaddock c2h docBase defaultNS (parseGtkDoc raw)
      writeHaddock DocAfterSymbol haddock

-- | Write the documentation for the given return value.
writeReturnDocumentation :: Callable -> Bool -> CodeGen e ()
writeReturnDocumentation callable skip = do
  c2h <- getC2HMap
  docBase <- getDocBase
  defaultNS <- modName <$> config
  let returnValInfo = if skip
                      then []
                      else case rawDocText (returnDocumentation callable) of
                             Nothing -> []
                             Just raw -> ["__Returns:__ " <>
                                           formatHaddock c2h docBase defaultNS
                                           (parseGtkDoc raw)]
      throwsInfo = if callableThrows callable
                   then ["/(Can throw 'Data.GI.Base.GError.GError')/"]
                   else []
  let fullInfo = T.intercalate " " (returnValInfo ++ throwsInfo)
  unless (T.null fullInfo) $
    writeHaddock DocAfterSymbol fullInfo

-- | Add the given text to the documentation for the section being generated.
addSectionDocumentation :: HaddockSection -> Documentation -> CodeGen e ()
addSectionDocumentation section doc = do
  c2h <- getC2HMap
  docBase <- getDocBase
  defaultNS <- modName <$> config
  let formatted = formatDocumentation c2h docBase defaultNS doc
  addSectionFormattedDocs section formatted
