{-# LANGUAGE ViewPatterns #-}
module GI.Overrides
    ( Overrides(pkgConfigMap, cabalPkgVersion, nsChooseVersion)
    , parseOverridesFile
    , filterAPIsAndDeps
    ) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer

import Data.Maybe (fromMaybe, isJust)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text (Text)

import GI.API

data Overrides = Overrides {
      -- | Prefix for constants in a given namespace, if not given the
      -- "_" string will be used.
      constantPrefix  :: M.Map String String,
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
      -- | Prefered version of the namespace
      nsChooseVersion :: M.Map String String
}

-- | Construct the generic config for a module.
defaultOverrides :: Overrides
defaultOverrides = Overrides {
              constantPrefix  = M.empty,
              ignoredElems    = M.empty,
              ignoredAPIs     = S.empty,
              sealedStructs   = S.empty,
              pkgConfigMap    = M.empty,
              cabalPkgVersion = Nothing,
              nsChooseVersion = M.empty }

-- | There is a sensible notion of zero and addition of Overridess,
-- encode this so that we can view the parser as a writer monad of
-- configs.
instance Monoid Overrides where
    mempty = defaultOverrides
    mappend a b = Overrides {
      constantPrefix = constantPrefix a <> constantPrefix b,
      ignoredAPIs = ignoredAPIs a <> ignoredAPIs b,
      sealedStructs = sealedStructs a <> sealedStructs b,
      ignoredElems = M.unionWith S.union (ignoredElems a) (ignoredElems b),
      pkgConfigMap = pkgConfigMap a <> pkgConfigMap b,
      cabalPkgVersion = if isJust (cabalPkgVersion b)
                        then cabalPkgVersion b
                        else cabalPkgVersion a,
      nsChooseVersion = nsChooseVersion a <> nsChooseVersion b
    }

-- | We have a bit of context (the current namespace), and can fail,
-- encode this in a monad.
type Parser = WriterT Overrides (StateT (Maybe String) (Except Text)) ()

-- | Parse the given config file (as a set of lines) for a given
-- introspection namespace, filling in the configuration as needed. In
-- case the parsing fails we return a description of the error
-- instead.
parseOverridesFile :: [Text] -> Either Text Overrides
parseOverridesFile ls = runExcept $ flip evalStateT Nothing $ execWriterT $
                                    mapM (parseOneLine . T.strip) ls

-- | Parse a single line of the config file, modifying the
-- configuration as appropriate.
parseOneLine :: Text -> Parser
-- Empty lines
parseOneLine line | T.null line = return ()
-- Comments
parseOneLine (T.stripPrefix "#" -> Just _) = return ()
parseOneLine (T.stripPrefix "namespace " -> Just ns) =
    (put . Just . T.unpack . T.strip) ns
parseOneLine (T.stripPrefix "ignore " -> Just ign) = get >>= parseIgnore ign
parseOneLine (T.stripPrefix "constantPrefix " -> Just p) = get >>= parseConstP p
parseOneLine (T.stripPrefix "seal " -> Just s) = get >>= parseSeal s
parseOneLine (T.stripPrefix "pkg-config-name" -> Just s) = parsePkgConfigName s
parseOneLine (T.stripPrefix "cabal-pkg-version" -> Just s) = parseCabalPkgVersion s
parseOneLine (T.stripPrefix "namespace-version" -> Just s) = parseNsVersion s
parseOneLine l = throwError $ "Could not understand \"" <> l <> "\"."

-- | Ignored elements.
parseIgnore :: Text -> Maybe String -> Parser
parseIgnore _ Nothing =
    throwError "'ignore' requires a namespace to be defined first."
parseIgnore (T.words -> [T.splitOn "." -> [api,elem]]) (Just ns) =
    tell $ defaultOverrides {ignoredElems = M.singleton (Name ns (T.unpack api))
                                         (S.singleton elem)}
parseIgnore (T.words -> [T.splitOn "." -> [api]]) (Just ns) =
    tell $ defaultOverrides {ignoredAPIs = S.singleton (Name ns (T.unpack api))}
parseIgnore ignore _ =
    throwError ("Ignore syntax is of the form \"ignore API.elem\" with '.elem' optional.\nGot \"ignore " <> ignore <> "\" instead.")

-- | Prefix for constants.
parseConstP :: Text -> Maybe String -> Parser
parseConstP _ Nothing = throwError "'constantPrefix' requires a namespace to be defined first. "
parseConstP (T.words -> [p]) (Just ns) = tell $
    defaultOverrides {constantPrefix = M.singleton ns (T.unpack p)}
parseConstP prefix _ =
    throwError ("constantPrefix syntax is of the form \"constantPrefix prefix\".\nGot \"constantPrefix " <> prefix <> "\" instead.")

-- | Sealed structures.
parseSeal :: Text -> Maybe String -> Parser
parseSeal _ Nothing = throwError "'seal' requires a namespace to be defined first."
parseSeal (T.words -> [s]) (Just ns) = tell $
    defaultOverrides {sealedStructs = S.singleton (Name ns (T.unpack s))}
parseSeal seal _ =
    throwError ("seal syntax is of the form \"seal name\".\nGot \"seal "
                <> seal <> "\" instead.")

-- | Mapping from GObject Introspection namespaces to pkg-config.
parsePkgConfigName :: Text -> Parser
parsePkgConfigName (T.words -> [gi,pc]) = tell $
    defaultOverrides {pkgConfigMap =
                          M.singleton (T.toLower gi) pc}
parsePkgConfigName t =
    throwError ("pkg-config-name syntax is of the form\n" <>
                "\t\"pkg-config-name gi-namespace pk-name\"\n" <>
                "Got \"pkg-config-name " <> t <> "\" instead.")

-- | Choose a preferred namespace version to load.
parseNsVersion :: Text -> Parser
parseNsVersion (T.words -> [ns,version]) = tell $
    defaultOverrides {nsChooseVersion =
                          M.singleton (T.unpack ns) (T.unpack version)}
parseNsVersion t =
    throwError ("namespace-version syntax is of the form\n" <>
                "\t\"namespace-version namespace version\"\n" <>
                "Got \"namespace-version " <> t <> "\" instead.")

-- | Specifying the cabal package version by hand.
parseCabalPkgVersion :: Text -> Parser
parseCabalPkgVersion (T.words -> [version]) = tell $
    defaultOverrides {cabalPkgVersion = Just version}
parseCabalPkgVersion t =
    throwError ("cabal-pkg-version syntax is of the form\n" <>
               "\t\"cabal-pkg-version version\"\n" <>
               "Got \"cabal-pkg-version " <> t <> "\" instead.")

-- | Filter a set of named objects based on a lookup list of names to
-- ignore.
filterNamed :: [(Name, a)] -> S.Set Text -> [(Name, a)]
filterNamed set ignores =
    filter ((`S.notMember` ignores) . T.pack . name . fst) set

-- | Filter one API according to the given config.
filterOneAPI :: Overrides -> (Name, API, Maybe (S.Set Text)) -> (Name, API)
filterOneAPI ovs (Name ns n, APIConst c, _) =
    (Name ns (prefix ++ n), APIConst c)
    where prefix = fromMaybe "_" $ M.lookup ns (constantPrefix ovs)
filterOneAPI ovs (n, APIStruct s, maybeIgnores) =
    (n, APIStruct s {structMethods = maybe (structMethods s)
                                     (filterNamed (structMethods s))
                                     maybeIgnores,
                     structFields = if n `S.member` sealedStructs ovs
                                    then []
                                    else structFields s})
-- The rest only apply if there are ignores.
filterOneAPI _ (n, api, Nothing) = (n, api)
filterOneAPI _ (n, APIObject o, Just ignores) =
    (n, APIObject o {objMethods = filterNamed (objMethods o) ignores,
                     objSignals = filter ((`S.notMember` ignores) . sigName)
                                  (objSignals o)
                    })
filterOneAPI _ (n, APIInterface i, Just ignores) =
    (n, APIInterface i {ifMethods = filterNamed (ifMethods i) ignores,
                        ifSignals = filter ((`S.notMember` ignores) . sigName)
                                    (ifSignals i)
                       })
filterOneAPI _ (n, APIUnion u, Just ignores) =
    (n, APIUnion u {unionMethods = filterNamed (unionMethods u) ignores})
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
