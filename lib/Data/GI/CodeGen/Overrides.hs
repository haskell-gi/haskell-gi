{-# LANGUAGE ViewPatterns #-}
module Data.GI.CodeGen.Overrides
    ( Overrides(pkgConfigMap, cabalPkgVersion, nsChooseVersion, girFixups)
    , parseOverridesFile
    , filterAPIsAndDeps
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Traversable (traverse)
#endif

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer

import Data.Maybe (isJust)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text (Text)

import qualified System.Info as SI
import qualified System.Environment as SE

import Data.GI.CodeGen.API
import qualified Text.XML as XML
import GI.GIR.XMLUtils (xmlLocalName, xmlNSName,
                        GIRXMLNamespace(CGIRNS, GLibGIRNS))

data Overrides = Overrides {
      -- | Ignored elements of a given API.
      ignoredElems    :: M.Map Name (S.Set Text),
      -- | Ignored APIs (all elements in this API will just be discarded).
      ignoredAPIs     :: S.Set Name,
      -- | Structs for which accessors should not be auto-generated.
      sealedStructs   :: S.Set Name,
      -- | Mapping from GObject Introspection namespaces to pkg-config
      pkgConfigMap    :: M.Map Text Text,
      -- | Version number for the generated .cabal package.
      cabalPkgVersion :: Maybe Text,
      -- | Prefered version of the namespace.
      nsChooseVersion :: M.Map Text Text,
      -- | Fixups for the GIR data.
      girFixups       :: [GIRRule]
} deriving (Show)

-- | Construct the generic config for a module.
defaultOverrides :: Overrides
defaultOverrides = Overrides {
              ignoredElems    = M.empty,
              ignoredAPIs     = S.empty,
              sealedStructs   = S.empty,
              pkgConfigMap    = M.empty,
              cabalPkgVersion = Nothing,
              nsChooseVersion = M.empty,
              girFixups       = [] }

-- | There is a sensible notion of zero and addition of Overridess,
-- encode this so that we can view the parser as a writer monad of
-- configs.
instance Monoid Overrides where
    mempty = defaultOverrides
    mappend a b = Overrides {
      ignoredAPIs = ignoredAPIs a <> ignoredAPIs b,
      sealedStructs = sealedStructs a <> sealedStructs b,
      ignoredElems = M.unionWith S.union (ignoredElems a) (ignoredElems b),
      pkgConfigMap = pkgConfigMap a <> pkgConfigMap b,
      cabalPkgVersion = if isJust (cabalPkgVersion b)
                        then cabalPkgVersion b
                        else cabalPkgVersion a,
      nsChooseVersion = nsChooseVersion a <> nsChooseVersion b,
      girFixups = girFixups a <> girFixups b
    }

-- | The state of the overrides parser.
data ParserState = ParserState {
      currentNS :: Maybe Text   -- ^ The current namespace.
    , flags     :: [ParserFlag] -- ^ Currently loaded flags.
    } deriving (Show)

-- | Default, empty, parser state.
emptyParserState :: ParserState
emptyParserState = ParserState {
                     currentNS = Nothing
                   , flags = []
                   }

-- | Conditional flags for the parser
data ParserFlag = FlagLinux
                | FlagOSX
                | FlagWindows
                  deriving (Show)

-- | Get the current namespace.
getNS :: Parser (Maybe Text)
getNS = currentNS <$> get

-- | Run the given parser only if the flags can be satisfied.
withFlags :: Parser () -> Parser ()
withFlags p = do
  fs <- flags <$> get
  check <- and <$> liftIO (traverse checkFlag fs)
  if check
  then p
  else return ()

-- | Check whether the given flag holds.
checkFlag :: ParserFlag -> IO Bool
checkFlag FlagLinux = checkOS "linux"
checkFlag FlagOSX = checkOS "darwin"
checkFlag FlagWindows = checkOS "mingw32"

-- | Check whether we are running under the given OS. We take the OS
-- from `System.Info.os`, but it is possible to override this value by
-- setting the environment variable @HASKELL_GI_OVERRIDE_OS@.
checkOS :: String -> IO Bool
checkOS os = SE.lookupEnv "HASKELL_GI_OVERRIDE_OS" >>= \case
             Nothing -> return (SI.os == os)
             Just ov -> return (ov == os)

-- | We have a bit of context (the current namespace), and can fail,
-- encode this in a monad.
type Parser a = WriterT Overrides (StateT ParserState (ExceptT Text IO)) a

-- | Parse the given config file (as a set of lines) for a given
-- introspection namespace, filling in the configuration as needed. In
-- case the parsing fails we return a description of the error
-- instead.
parseOverridesFile :: [Text] -> IO (Either Text Overrides)
parseOverridesFile ls =
    runExceptT $ flip evalStateT emptyParserState $ execWriterT $
              mapM (parseOneLine . T.strip) ls

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
parseOneLine (T.stripPrefix "pkg-config-name " -> Just s) =
    withFlags $ parsePkgConfigName s
parseOneLine (T.stripPrefix "cabal-pkg-version " -> Just s) =
    withFlags $ parseCabalPkgVersion s
parseOneLine (T.stripPrefix "namespace-version " -> Just s) =
    withFlags $ parseNsVersion s
parseOneLine (T.stripPrefix "set-attr " -> Just s) =
    withFlags $ parseSetAttr s
parseOneLine (T.stripPrefix "if " -> Just s) =
    withFlags $ parseIf s
parseOneLine (T.stripPrefix "endif" -> Just s) = parseEndif s
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

-- | Parse a path specification, which is of the form
-- "nodeSpec1/nodeSpec2/../nodeSpecN", where nodeSpec is a node
-- specification of the form "nodeType[:name attribute]".
parsePathSpec :: Text -> Parser GIRPath
parsePathSpec spec = mapM parseNodeSpec (T.splitOn "/" spec)

-- | Parse a single node specification.
parseNodeSpec :: Text -> Parser GIRNodeSpec
parseNodeSpec spec = case T.splitOn "@" spec of
                       [n] -> return (GIRNamed n)
                       ["", t] -> return (GIRType t)
                       [n, t] -> return (GIRTypedName t n)
                       _ -> throwError ("Could not understand node spec \""
                                        <> spec <> "\".")

-- | Parse an XML name, with an optional prefix.
parseXMLName :: Text -> Parser XML.Name
parseXMLName a = case T.splitOn ":" a of
                   [n] -> return (xmlLocalName n)
                   ["c", n] -> return (xmlNSName CGIRNS n)
                   ["glib", n] -> return (xmlNSName GLibGIRNS n)
                   _ -> throwError ("Could not understand xml name \""
                                    <> a <> "\".")

-- | Parse a 'if' directive.
parseIf :: Text -> Parser ()
parseIf cond = case T.words cond of
                 [] -> throwError ("Empty 'if' condition.")
                 ["linux"] -> setFlag FlagLinux
                 ["osx"] -> setFlag FlagOSX
                 ["windows"] -> setFlag FlagWindows
                 _ -> throwError ("Unknown condition \"" <> cond <> "\".")
    where setFlag :: ParserFlag -> Parser ()
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

-- | Filter a set of named objects based on a lookup list of names to
-- ignore.
filterMethods :: [Method] -> S.Set Text -> [Method]
filterMethods set ignores =
    filter ((`S.notMember` ignores) . name . methodName) set

-- | Filter one API according to the given config.
filterOneAPI :: Overrides -> (Name, API, Maybe (S.Set Text)) -> (Name, API)
filterOneAPI ovs (n, APIStruct s, maybeIgnores) =
    (n, APIStruct s {structMethods = maybe (structMethods s)
                                     (filterMethods (structMethods s))
                                     maybeIgnores,
                     structFields = if n `S.member` sealedStructs ovs
                                    then []
                                    else structFields s})
-- The rest only apply if there are ignores.
filterOneAPI _ (n, api, Nothing) = (n, api)
filterOneAPI _ (n, APIObject o, Just ignores) =
    (n, APIObject o {objMethods = filterMethods (objMethods o) ignores,
                     objSignals = filter ((`S.notMember` ignores) . sigName)
                                  (objSignals o)
                    })
filterOneAPI _ (n, APIInterface i, Just ignores) =
    (n, APIInterface i {ifMethods = filterMethods (ifMethods i) ignores,
                        ifSignals = filter ((`S.notMember` ignores) . sigName)
                                    (ifSignals i)
                       })
filterOneAPI _ (n, APIUnion u, Just ignores) =
    (n, APIUnion u {unionMethods = filterMethods (unionMethods u) ignores})
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
