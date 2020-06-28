{-# LANGUAGE ViewPatterns #-}
module Data.GI.CodeGen.Overrides
    ( Overrides(pkgConfigMap, cabalPkgVersion, nsChooseVersion, girFixups,
                onlineDocsMap)
    , parseOverrides
    , filterAPIsAndDeps
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Traversable (traverse)
#endif

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer (WriterT, execWriterT, tell)

import Data.Maybe (isJust)
import qualified Data.Map as M
import Data.Semigroup as Sem
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Version as V

import Text.ParserCombinators.ReadP (readP_to_S)

import qualified System.Info as SI

import Data.GI.CodeGen.API
import qualified Text.XML as XML
import Data.GI.CodeGen.PkgConfig (tryPkgConfig)
import Data.GI.CodeGen.Util (tshow, utf8ReadFile)
import Data.GI.GIR.XMLUtils (xmlLocalName, xmlNSName,
                             GIRXMLNamespace(CGIRNS, GLibGIRNS, CoreGIRNS))

data Overrides = Overrides {
      -- | Ignored elements of a given API.
      ignoredElems    :: M.Map Name (S.Set Text),
      -- | Ignored APIs (all elements in this API will just be discarded).
      ignoredAPIs     :: S.Set Name,
      -- | Structs for which accessors should not be auto-generated.
      sealedStructs   :: S.Set Name,
      -- | Explicit calloc\/copy\/free for structs/unions.
      allocInfo       :: M.Map Name AllocationInfo,
      -- | Mapping from GObject Introspection namespaces to pkg-config
      pkgConfigMap    :: M.Map Text Text,
      -- | Version number for the generated .cabal package.
      cabalPkgVersion :: Maybe Text,
      -- | Prefered version of the namespace.
      nsChooseVersion :: M.Map Text Text,
      -- | Fixups for the GIR data.
      girFixups       :: [GIRRule],
      -- | Known places where to find the C docs.
      onlineDocsMap   :: M.Map Text Text
} deriving (Show)

-- | Construct the generic config for a module.
defaultOverrides :: Overrides
defaultOverrides = Overrides {
                     ignoredElems    = M.empty,
                     ignoredAPIs     = S.empty,
                     sealedStructs   = S.empty,
                     allocInfo       = M.empty,
                     pkgConfigMap    = M.empty,
                     cabalPkgVersion = Nothing,
                     nsChooseVersion = M.empty,
                     girFixups       = [],
                     onlineDocsMap   = M.empty
                   }

-- | There is a sensible notion of zero and addition of Overridess,
-- encode this so that we can view the parser as a writer monad of
-- configs.
instance Monoid Overrides where
  mempty = defaultOverrides
#if !MIN_VERSION_base(4,11,0)
  mappend = concatOverrides
#endif

-- | There is a sensible notion of zero and addition of Overridess,
-- encode this so that we can view the parser as a writer monad of
-- configs.
instance Sem.Semigroup Overrides where
  (<>) = concatOverrides

-- | Addition of overrides is meaningful.
concatOverrides :: Overrides -> Overrides -> Overrides
concatOverrides a b = Overrides {
      ignoredAPIs = ignoredAPIs a <> ignoredAPIs b,
      sealedStructs = sealedStructs a <> sealedStructs b,
      allocInfo = allocInfo a <> allocInfo b,
      ignoredElems = M.unionWith S.union (ignoredElems a) (ignoredElems b),
      pkgConfigMap = pkgConfigMap a <> pkgConfigMap b,
      cabalPkgVersion = if isJust (cabalPkgVersion b)
                        then cabalPkgVersion b
                        else cabalPkgVersion a,
      nsChooseVersion = nsChooseVersion a <> nsChooseVersion b,
      girFixups = girFixups a <> girFixups b,
      onlineDocsMap = onlineDocsMap a <> onlineDocsMap b
    }

-- | The state of the overrides parser.
data ParserState = ParserState {
      currentNS :: Maybe Text   -- ^ The current namespace.
    , flags     :: [Bool] -- ^ The contents of the override file will
                          -- be ignored if there is any `False` value
                          -- here. @if@ primitive push (prepend)
                          -- values here, @endif@ pop them.
    } deriving (Show)

-- | Default, empty, parser state.
emptyParserState :: ParserState
emptyParserState = ParserState {
                     currentNS = Nothing
                   , flags = []
                   }

-- | Get the current namespace.
getNS :: Parser (Maybe Text)
getNS = currentNS <$> get

-- | Run the given parser only if the flags can be satisfied.
withFlags :: Parser () -> Parser ()
withFlags p = do
  fs <- flags <$> get
  if and fs
  then p
  else return ()

-- | We have a bit of context (the current namespace), and can fail,
-- encode this in a monad.
type Parser a = WriterT Overrides (StateT ParserState (ExceptT Text IO)) a

-- | Parse the given overrides, filling in the configuration as
-- needed. In case the parsing fails we return a description of the
-- error instead.
parseOverrides :: Text -> IO (Either Text Overrides)
parseOverrides overrides = do
  runExceptT $ flip evalStateT emptyParserState $ execWriterT $
    mapM (parseOneLine . T.strip) (T.lines overrides)

-- | Parse a single line of the config file, modifying the
-- configuration as appropriate.
parseOneLine :: Text -> Parser ()
-- Empty lines
parseOneLine line | T.null line = return ()
-- Comments
parseOneLine (T.stripPrefix "#" -> Just _) = return ()
parseOneLine (T.stripPrefix "namespace " -> Just ns) =
    withFlags $ modify' (\s -> s {currentNS = (Just . T.strip) ns})
parseOneLine (T.stripPrefix "ignore " -> Just ign) =
    withFlags $ getNS >>= parseIgnore ign
parseOneLine (T.stripPrefix "seal " -> Just s) =
    withFlags $ getNS >>= parseSeal s
parseOneLine (T.stripPrefix "alloc-info " -> Just s) =
    withFlags $ getNS >>= parseAllocInfo s
parseOneLine (T.stripPrefix "pkg-config-name " -> Just s) =
    withFlags $ parsePkgConfigName s
parseOneLine (T.stripPrefix "cabal-pkg-version " -> Just s) =
    withFlags $ parseCabalPkgVersion s
parseOneLine (T.stripPrefix "namespace-version " -> Just s) =
    withFlags $ parseNsVersion s
parseOneLine (T.stripPrefix "set-attr " -> Just s) =
    withFlags $ parseSetAttr s
parseOneLine (T.stripPrefix "delete-attr " -> Just s) =
    withFlags $ parseDeleteAttr s
parseOneLine (T.stripPrefix "add-node " -> Just s) =
    withFlags $ parseAdd s
parseOneLine (T.stripPrefix "delete-node " -> Just s) =
    withFlags $ parseDelete s
parseOneLine (T.stripPrefix "C-docs-url " -> Just u) =
    withFlags $ parseDocsUrl u
parseOneLine (T.stripPrefix "if " -> Just s) = parseIf s
parseOneLine (T.stripPrefix "endif" -> Just s) = parseEndif s
parseOneLine (T.stripPrefix "include " -> Just s) = parseInclude s
parseOneLine l = throwError $ "Could not understand \"" <> l <> "\"."

-- | Ignored elements.
parseIgnore :: Text -> Maybe Text -> Parser ()
parseIgnore _ Nothing =
    throwError "'ignore' requires a namespace to be defined first."
parseIgnore (T.words -> [T.splitOn "." -> [api,elem]]) (Just ns) =
    tell $ defaultOverrides {ignoredElems = M.singleton (Name ns api)
                                         (S.singleton elem)}
parseIgnore (T.words -> [T.splitOn "." -> [api]]) (Just ns) =
    tell $ defaultOverrides {ignoredAPIs = S.singleton (Name ns api)}
parseIgnore ignore _ =
    throwError ("Ignore syntax is of the form \"ignore API.elem\" with '.elem' optional.\nGot \"ignore " <> ignore <> "\" instead.")

-- | Sealed structures.
parseSeal :: Text -> Maybe Text -> Parser ()
parseSeal _ Nothing = throwError "'seal' requires a namespace to be defined first."
parseSeal (T.words -> [s]) (Just ns) = tell $
    defaultOverrides {sealedStructs = S.singleton (Name ns s)}
parseSeal seal _ =
    throwError ("seal syntax is of the form \"seal name\".\nGot \"seal "
                <> seal <> "\" instead.")

-- | Explicit allocation info for wrapped pointers.
parseAllocInfo :: Text -> Maybe Text -> Parser ()
parseAllocInfo _ Nothing = throwError "'alloc-info' requires a namespace to be defined first."
parseAllocInfo (T.words -> (n:ops)) (Just ns) = do
  parsedOps <- traverse parseKeyValuePair ops
  info <- foldM applyOp unknownAllocationInfo parsedOps
  tell $ defaultOverrides {allocInfo = M.singleton (Name ns n) info}
  where applyOp :: AllocationInfo -> (Text, Text) -> Parser AllocationInfo
        applyOp a ("calloc", f) = return (a {allocCalloc = AllocationOp f})
        applyOp a ("copy", f) = return (a {allocCopy = AllocationOp f})
        applyOp a ("free", f) = return (a {allocFree = AllocationOp f})
        applyOp _ (op, _) = throwError ("Unknown alloc op \"" <> op <> "\".")
parseAllocInfo info _ =
    throwError ("alloc-info syntax is of the form "
                <> "\"alloc-info name calloc copy free\", with \"-\" meaning "
                <> "a masked operation. Got \"alloc-info " <> info
                <> "\" instead.")

-- | Parse a explicit key=value pair into a (key, value) tuple.
parseKeyValuePair :: Text -> Parser (Text, Text)
parseKeyValuePair p =
    case T.splitOn "=" p of
      [k,v] -> return (k, v)
      _ -> throwError ("Could not parse \"" <> p <> "\"as a \"key=value\" pair.")

-- | Mapping from GObject Introspection namespaces to pkg-config.
parsePkgConfigName :: Text -> Parser ()
parsePkgConfigName (T.words -> [gi,pc]) = tell $
    defaultOverrides {pkgConfigMap =
                          M.singleton (T.toLower gi) pc}
parsePkgConfigName t =
    throwError ("pkg-config-name syntax is of the form\n" <>
                "\t\"pkg-config-name gi-namespace pk-name\"\n" <>
                "Got \"pkg-config-name " <> t <> "\" instead.")

-- | Choose a preferred namespace version to load.
parseNsVersion :: Text -> Parser ()
parseNsVersion (T.words -> [ns,version]) = tell $
    defaultOverrides {nsChooseVersion =
                          M.singleton ns version}
parseNsVersion t =
    throwError ("namespace-version syntax is of the form\n" <>
                "\t\"namespace-version namespace version\"\n" <>
                "Got \"namespace-version " <> t <> "\" instead.")

-- | Specifying the cabal package version by hand.
parseCabalPkgVersion :: Text -> Parser ()
parseCabalPkgVersion (T.words -> [version]) = tell $
    defaultOverrides {cabalPkgVersion = Just version}
parseCabalPkgVersion t =
    throwError ("cabal-pkg-version syntax is of the form\n" <>
               "\t\"cabal-pkg-version version\"\n" <>
               "Got \"cabal-pkg-version " <> t <> "\" instead.")

-- | Set a given attribute in the GIR file.
parseSetAttr :: Text -> Parser ()
parseSetAttr (T.words -> [path, attr, newVal]) = do
  pathSpec <- parsePathSpec path
  parsedAttr <- parseXMLName attr
  tell $ defaultOverrides {girFixups =
                           [GIRSetAttr (pathSpec, parsedAttr) newVal]}
parseSetAttr t =
    throwError ("set-attr syntax is of the form\n" <>
               "\t\"set-attr nodePath attrName newValue\"\n" <>
               "Got \"set-attr " <> t <> "\" instead.")

-- | Delete the given attribute
parseDeleteAttr :: Text -> Parser ()
parseDeleteAttr (T.words -> [path, attr]) = do
  pathSpec <- parsePathSpec path
  parsedAttr <- parseXMLName attr
  tell $ defaultOverrides {girFixups = [GIRDeleteAttr pathSpec parsedAttr]}
parseDeleteAttr t =
    throwError ("delete-attr syntax is of the form\n" <>
               "\t\"delete-attr nodePath attrName\"\n" <>
               "Got \"delete-attr " <> t <> "\" instead.")

-- | Add the given child node to all nodes matching the path.
parseAdd :: Text -> Parser ()
parseAdd (T.words -> [path, name]) = do
  pathSpec <- parsePathSpec path
  parsedName <- parseXMLName name
  tell $ defaultOverrides {girFixups = [GIRAddNode pathSpec parsedName]}
parseAdd t =
    throwError ("add-node syntax is of the form\n" <>
               "\t\"add-node nodePath newName\"\n" <>
               "Got \"add-node " <> t <> "\" instead.")

-- | Delete all nodes matching the given path.
parseDelete :: Text -> Parser ()
parseDelete (T.words -> [path]) = do
  pathSpec <- parsePathSpec path
  tell $ defaultOverrides {girFixups = [GIRDeleteNode pathSpec]}
parseDelete t =
    throwError ("delete-node syntax is of the form\n" <>
               "\t\"delete-node nodePath\"\n" <>
               "Got \"delete-node " <> t <> "\" instead.")

-- | Parse a documentation URL for the given module.
parseDocsUrl :: Text -> Parser ()
parseDocsUrl (T.words -> [ns, url]) = do
  tell $ defaultOverrides { onlineDocsMap = M.singleton ns url }
parseDocsUrl t =
  throwError ("C-docs-url syntax of of the form\n" <>
              "\t\"C-docs-url namespace url\"\n" <>
              "Got \"C-docs-url " <> t <> "\" instead.")

-- | Parse a path specification, which is of the form
-- "nodeSpec1/nodeSpec2/../nodeSpecN", where nodeSpec is a node
-- specification of the form "nodeType[:name attribute]".
parsePathSpec :: Text -> Parser GIRPath
parsePathSpec spec = mapM parseNodeSpec (T.splitOn "/" spec)

-- | A specification of a name, which is either a regex (prefixed with
-- "~") or a plain name.
parseGIRNameTag :: Text -> GIRNameTag
parseGIRNameTag (T.stripPrefix "~" -> Just regex) = GIRRegex regex
parseGIRNameTag t = GIRPlainName t

-- | Parse a single node specification.
parseNodeSpec :: Text -> Parser GIRNodeSpec
parseNodeSpec spec = case T.splitOn "@" spec of
                       [n] -> return (GIRNamed (parseGIRNameTag n))
                       ["", t] -> return (GIRType t)
                       [n, t] -> return (GIRTypedName t (parseGIRNameTag n))
                       _ -> throwError ("Could not understand node spec \""
                                        <> spec <> "\".")

-- | Parse an XML name, with an optional prefix.
parseXMLName :: Text -> Parser XML.Name
parseXMLName a = case T.splitOn ":" a of
                   [n] -> return (xmlLocalName n)
                   ["c", n] -> return (xmlNSName CGIRNS n)
                   ["glib", n] -> return (xmlNSName GLibGIRNS n)
                   ["core", n] -> return (xmlNSName CoreGIRNS n)
                   _ -> throwError ("Could not understand xml name \""
                                    <> a <> "\".")

-- | Known operating systems.
data OSType = Linux
            | OSX
            | Windows
              deriving (Show)

-- | Check whether we are running under the given OS.
checkOS :: String -> Parser Bool
checkOS os = return (SI.os == os)

-- | Parse a textual representation of a version into a `Data.Version.Version`.
parseVersion :: Text -> Parser V.Version
parseVersion v = (chooseFullParse . readP_to_S V.parseVersion . T.unpack) v
    where chooseFullParse :: [(V.Version, String)] -> Parser V.Version
          chooseFullParse [] = throwError ("Could not parse version \""
                                           <> v <> "\".")
          chooseFullParse [(parsed, "")] = return parsed
          chooseFullParse (_ : rest) = chooseFullParse rest

-- | Check that the given pkg-config package has a version compatible
-- with the given constraint.
checkPkgConfigVersion :: Text -> Text -> Text -> Parser Bool
checkPkgConfigVersion pkg op tVersion = do
  version <- parseVersion tVersion
  pcVersion <- liftIO (tryPkgConfig pkg) >>= \case
               Nothing ->
                   throwError ("Could not determine pkg-config version for \""
                               <> pkg <> "\".")
               Just (_, tv) -> parseVersion tv
  case op of
    "==" -> return (pcVersion == version)
    "/=" -> return (pcVersion /= version)
    ">=" -> return (pcVersion >= version)
    ">"  -> return (pcVersion >  version)
    "<=" -> return (pcVersion <= version)
    "<"  -> return (pcVersion <  version)
    _    -> throwError ("Unrecognized comparison operator \"" <> op <> "\".")

-- | Parse a 'if' directive.
parseIf :: Text -> Parser ()
parseIf cond = case T.words cond of
                 [] -> throwError ("Empty 'if' condition.")
                 ["linux"] -> checkOS "linux" >>= setFlag
                 ["osx"] -> checkOS "darwin" >>= setFlag
                 ["windows"] -> checkOS "mingw32" >>= setFlag
                 ("pkg-config-version" : rest) ->
                     case rest of
                       [pkg, op, version] ->
                           checkPkgConfigVersion pkg op version >>= setFlag
                       _ -> throwError ("Syntax for `pkg-config-version' is "
                                        <> "\"pkg op version\", got \""
                                        <> tshow rest <> "\".")
                 _ -> throwError ("Unknown condition \"" <> cond <> "\".")
    where setFlag :: Bool -> Parser ()
          setFlag flag = modify' (\s -> s {flags = flag : flags s})

-- | Parse an 'endif' directive.
parseEndif :: Text -> Parser ()
parseEndif rest = case T.words rest of
                    [] -> unsetFlag
                    _ -> throwError ("Unexpected argument to 'endif': \""
                                     <> rest <> "\".")
    where unsetFlag :: Parser ()
          unsetFlag = do
            s <- get
            case flags s of
              _:rest -> put (s {flags = rest})
              [] -> throwError ("'endif' with no matching 'if'.")

-- | Parse the given overrides file, and merge into the given context.
parseInclude :: Text -> Parser ()
parseInclude fname = do
  includeText <- liftIO $ utf8ReadFile (T.unpack fname)
  liftIO (parseOverrides includeText) >>= \case
    Left err -> throwError ("Error when parsing included '"
                            <> fname <> "': " <> err)
    Right ovs -> tell ovs

-- | Filter a set of named objects based on a lookup list of names to
-- ignore.
filterMethods :: [Method] -> S.Set Text -> [Method]
filterMethods set ignores =
    filter ((`S.notMember` ignores) . name . methodName) set

-- | Given the previous allocation info, and a new allocation info,
-- replace those entries in the old allocation info which are
-- specified in the new info.
filterAllocInfo :: AllocationInfo -> AllocationInfo -> AllocationInfo
filterAllocInfo old new =
    AllocationInfo { allocCalloc = replace (allocCalloc old) (allocCalloc new)
                   , allocCopy = replace (allocCopy old) (allocCopy new)
                   , allocFree = replace (allocFree old) (allocFree new) }
    where replace :: AllocationOp -> AllocationOp -> AllocationOp
          replace o AllocationOpUnknown = o
          replace _ o = o

-- | Filter one API according to the given config.
filterOneAPI :: Overrides -> (Name, API, Maybe (S.Set Text)) -> (Name, API)
filterOneAPI ovs (n, APIStruct s, maybeIgnores) =
    (n, APIStruct s { structMethods = maybe (structMethods s)
                                      (filterMethods (structMethods s))
                                      maybeIgnores
                    , structFields = if n `S.member` sealedStructs ovs
                                    then []
                                    else structFields s
                    , structAllocationInfo =
                        let ai = structAllocationInfo s
                        in case M.lookup n (allocInfo ovs) of
                             Just info -> filterAllocInfo ai info
                             Nothing -> ai
                    })
filterOneAPI ovs (n, APIUnion u, maybeIgnores) =
    (n, APIUnion u {unionMethods = maybe (unionMethods u)
                                   (filterMethods (unionMethods u))
                                   maybeIgnores
                   , unionAllocationInfo =
                        let ai = unionAllocationInfo u
                        in case M.lookup n (allocInfo ovs) of
                             Just info -> filterAllocInfo ai info
                             Nothing -> ai
                   })
-- The rest only apply if there are ignores.
filterOneAPI _ (n, api, Nothing) = (n, api)
filterOneAPI _ (n, APIObject o, Just ignores) =
    (n, APIObject o {objMethods = filterMethods (objMethods o) ignores,
                     objSignals = filter ((`S.notMember` ignores) . sigName)
                                  (objSignals o)
                    })
filterOneAPI ovs (n, APIInterface i, Just ignores) =
    (n, APIInterface i {ifMethods = filterMethods (ifMethods i) ignores,
                        ifSignals = filter ((`S.notMember` ignores) . sigName)
                                    (ifSignals i),
                        ifAllocationInfo =
                           let ai = ifAllocationInfo i
                           in case M.lookup n (allocInfo ovs) of
                                Just info -> filterAllocInfo ai info
                                Nothing -> ai

                       })
filterOneAPI _ (n, api, _) = (n, api)

-- | Given a list of APIs modify them according to the given config.
filterAPIs :: Overrides -> [(Name, API)] -> [(Name, API)]
filterAPIs ovs apis = map (filterOneAPI ovs . fetchIgnores) filtered
    where filtered = filter ((`S.notMember` ignoredAPIs ovs) . fst) apis
          fetchIgnores (n, api) = (n, api, M.lookup n (ignoredElems ovs))

-- | Load a given API, applying filtering. Load also any necessary
-- dependencies.
filterAPIsAndDeps :: Overrides -> GIRInfo -> [GIRInfo]
                  -> (M.Map Name API, M.Map Name API)
filterAPIsAndDeps ovs doc deps =
  let toMap = M.fromList . filterAPIs ovs . girAPIs
  in (toMap doc, M.unions (map toMap deps))
