module Data.GI.CodeGen.PkgConfig
    ( pkgConfigGetVersion
    , tryPkgConfig
    , pkgConfigModVersions
    , TaggedPkgConfigVersions(..)
    , checkDepVersions
    ) where

import Control.Monad (when)
import qualified Data.List as L
import Data.Maybe (catMaybes)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid (First(..), (<>))
#else
import Data.Monoid (First(..))
#endif
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text (Text)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..), exitFailure)
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

-- | The versions of the pkg-config packages that the bindings for a
-- dependency were generated against, as recorded in the
-- @pkgConfigVersions@ entry of its @GI.<Name>.Config@ module.
data TaggedPkgConfigVersions =
  TaggedPkgConfigVersions { pkgConfigTag :: Text
                            -- ^ Tag for the dependency, for error
                            -- reporting purposes.
                          , pkgConfigVersionMap :: M.Map Text Text
                            -- ^ Map from pkg-config package names to
                            -- the version of the package at the time
                            -- the dependency bindings were generated.
                          }

-- | Try asking pkg-config for the version of a given module, and
-- return the package name together with its version.
tryPkgConfig :: Text -> IO (Maybe (Text, Text))
tryPkgConfig pkgName = do
  (exitcode, stdout, _) <-
      readProcessWithExitCode "pkg-config" ["--modversion", T.unpack pkgName] ""
  case exitcode of
    ExitSuccess -> case lines stdout of
                     [v] -> return (Just (pkgName, T.pack v))
                     _ -> return Nothing
    ExitFailure _ -> return Nothing

-- | Ask pkg-config for the installed version of each of the given
-- packages. Packages unknown to pkg-config are omitted from the
-- result.
pkgConfigModVersions :: [Text] -> IO (M.Map Text Text)
pkgConfigModVersions pkgs =
  M.fromList . catMaybes <$> mapM tryPkgConfig pkgs

-- | Get the pkg-config name and associated installed version of a given
-- gobject-introspection namespace. Since the mapping is not
-- one-to-one some guessing is involved, although in most cases the
-- required information is listed in the GIR file.
pkgConfigGetVersion :: Text     -- name
                    -> Text     -- version
                    -> [Text]   -- known package names
                    -> Bool     -- verbose
                    -> M.Map Text Text -- suggested overrides
                    -> IO (Maybe (Text, Text))
pkgConfigGetVersion name version packages verbose overridenNames = do
  let lowerName = T.toLower name
  when verbose $
           putStrLn $ T.unpack ("Querying pkg-config for " <> name <>
                              " version " <> version)
  let alternatives = case M.lookup lowerName overridenNames of
                       Nothing -> packages ++ [lowerName <> "-" <> version,
                                               lowerName]
                       Just n -> [n <> "-" <> version, n]
      firstJust = getFirst . mconcat . map First
  mapM tryPkgConfig alternatives >>= return . firstJust

-- | What to do when the versions of the pkg-config packages that the
-- dependencies were generated against do not agree with the
-- currently installed ones.
data OnVersionMismatch = IgnoreMismatch -- ^ Do nothing.
                       | WarnOnMismatch -- ^ Print a warning and continue.
                       | AbortOnMismatch -- ^ Print a warning and abort.

-- | Name of the environment variable controlling what to do on a
-- version mismatch.
onVersionMismatchEnvVar :: String
onVersionMismatchEnvVar = "HASKELL_GI_ON_VERSION_MISMATCH"

-- | Read the desired behaviour on version mismatches from the
-- environment, defaulting to `WarnOnMismatch`.
getOnVersionMismatch :: IO OnVersionMismatch
getOnVersionMismatch =
  lookupEnv onVersionMismatchEnvVar >>= \case
    Nothing -> return WarnOnMismatch
    Just v -> case T.toLower (T.strip (T.pack v)) of
      "" -> return WarnOnMismatch
      "ignore" -> return IgnoreMismatch
      "warn" -> return WarnOnMismatch
      "abort" -> return AbortOnMismatch
      _ -> do
        hPutStrLn stderr $ "Error: unknown value \"" <> v <> "\" for "
          <> onVersionMismatchEnvVar
          <> ". Valid values are \"ignore\", \"warn\" (the default) and \"abort\"."
        exitFailure

-- | Check that the versions of the pkg-config packages that the
-- bindings for the dependencies were generated against agree with
-- the versions currently reported by pkg-config for the given
-- packages (typically, those listed in the loaded GIR files for the
-- dependencies). What happens if they do not is controlled by the
-- @HASKELL_GI_ON_VERSION_MISMATCH@ environment variable, see
-- `getOnVersionMismatch`.
checkDepVersions :: Text -- ^ Name of the package being generated
                 -> Bool -- ^ verbose
                 -> [Text] -- ^ pkg-config packages to check
                 -> [TaggedPkgConfigVersions] -- ^ Dependency versions
                 -> IO ()
checkDepVersions pkgName verbosity packages deps =
  getOnVersionMismatch >>= \case
    IgnoreMismatch -> return ()
    WarnOnMismatch -> findMismatches >>= \case
      [] -> return ()
      mismatches -> printWarning mismatches
        [ "Continuing anyway. Set " <> envVar <> "=abort to make this"
        , "an error, or " <> envVar <> "=ignore to silence this warning." ]
    AbortOnMismatch -> findMismatches >>= \case
      [] -> return ()
      mismatches -> do
        printWarning mismatches
          [ "Aborting, since " <> envVar <> "=abort."]
        exitFailure

  where
    envVar :: Text
    envVar = T.pack onVersionMismatchEnvVar

    findMismatches :: IO [(Text, Text, Text, Text)]
    findMismatches = do
      let recorded = M.unions (map pkgConfigVersionMap deps)
          pcPkgs = filter (`M.member` recorded) (L.nub packages)
      current <- pkgConfigModVersions pcPkgs

      when verbosity $
        putStrLn $ "Checking dependency versions against pkg-config: "
        <> show (M.toList current)

      return [ (tag, pkg, recorded, installed)
             | TaggedPkgConfigVersions tag versions <- deps
             , (pkg, recorded) <- M.toList versions
             , Just installed <- [M.lookup pkg current]
             , installed /= recorded ]

    printWarning :: [(Text, Text, Text, Text)] -> [Text] -> IO ()
    printWarning mismatches outcome =
      hPutStr stderr . T.unpack $ T.unlines $
        [ banner
        , "WARNING: VERSION MISMATCH WHILE GENERATING BINDINGS FOR " <> T.toUpper pkgName
        , banner
        , ""
        , "The Haskell bindings for the following dependencies were generated"
        , "against a different version of the underlying library than the one"
        , "currently reported by pkg-config:"
        , ""
        ] <>
        [ "  * " <> tag <> ": generated against " <> pkg <> " " <> recorded
          <> ", but pkg-config reports " <> pkg <> " " <> installed
        | (tag, pkg, recorded, installed) <- mismatches ] <>
        [ ""
        , "The bindings for " <> pkgName <> " are generated from the currently"
        , "installed introspection data, so they may refer to types, methods"
        , "or functions that are missing from the dependency bindings listed"
        , "above. This typically shows up later as compilation errors of the"
        , "form \"Not in scope: type constructor or class ...\" or \"module ..."
        , "does not export ...\"."
        , ""
        , "If this happens, rebuild the dependency bindings listed above"
        , "against the currently installed libraries."
        , ""
        ] <> outcome <>
        [ banner ]

    banner :: Text
    banner = T.replicate 72 "*"
