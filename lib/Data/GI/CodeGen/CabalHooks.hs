-- | Convenience hooks for writing custom @Setup.hs@ files for
-- bindings.
module Data.GI.CodeGen.CabalHooks
    ( setupHaskellGIBinding
    , setupBinding
    , configureDryRun
    , TaggedOverride(..)
    ) where

import qualified Distribution.ModuleName as MN
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple (UserHooks(..), simpleUserHooks,
                            defaultMainWithHooks, OptimisationLevel(..))
import Distribution.PackageDescription

import Data.GI.CodeGen.API (loadGIRInfo)
import Data.GI.CodeGen.Code (genCode, writeModuleTree, listModuleTree,
                             ModuleInfo, transitiveModuleDeps)
import Data.GI.CodeGen.CodeGen (genModule)
import Data.GI.CodeGen.Config (Config(..))
import Data.GI.CodeGen.LibGIRepository (setupTypelibSearchPath)
import Data.GI.CodeGen.ModulePath (toModulePath)
import Data.GI.CodeGen.Overrides (parseOverrides, girFixups,
                                  filterAPIsAndDeps)
import Data.GI.CodeGen.Util (utf8ReadFile, utf8WriteFile, ucFirst)

import System.Directory (createDirectoryIfMissing)
import System.FilePath (joinPath, takeDirectory)

import Control.Monad (void, forM)

import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map as M
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

type ConfHook = (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags
              -> IO LocalBuildInfo

-- | Included overrides file.
data TaggedOverride =
  TaggedOverride { overrideTag   :: Text
                   -- ^ Tag for the override, for error reporting purposes.
                 , overrideText  :: Text
                 }

-- | Generate the code for the given module.
genModuleCode :: Text -- ^ name
              -> Text -- ^ version
              -> Bool -- ^ verbose
              -> [TaggedOverride] -- ^ Explicit overrides
              -> IO ModuleInfo
genModuleCode name version verbosity overrides = do
  setupTypelibSearchPath []

  parsed <- forM overrides $ \(TaggedOverride tag ovText) -> do
    parseOverrides ovText >>= \case
      Left err -> error $ "Error when parsing overrides file \""
                  <> T.unpack tag <> "\":"
                  <> T.unpack err
      Right ovs -> return ovs

  let ovs = mconcat parsed

  (gir, girDeps) <- loadGIRInfo verbosity name (Just version) [] (girFixups ovs)
  let (apis, deps) = filterAPIsAndDeps ovs gir girDeps
      allAPIs = M.union apis deps
      cfg = Config {modName = name,
                    verbose = verbosity,
                    overrides = ovs}

  return $ genCode cfg allAPIs (toModulePath name) (genModule apis)

-- | Write a module containing information about the configuration for
-- the package.
genConfigModule :: Maybe FilePath -> Text -> Maybe TaggedOverride -> IO ()
genConfigModule outputDir modName maybeGiven = do
  let fname = joinPath [ fromMaybe "" outputDir
                       , "GI"
                       , T.unpack (ucFirst modName)
                       , "Config.hs" ]
      dirname = takeDirectory fname

  createDirectoryIfMissing True dirname

  utf8WriteFile fname $ T.unlines
    [ "{-# LANGUAGE OverloadedStrings #-}"
    , "-- | Build time configuration used during code generation."
    , "module GI." <> ucFirst modName <> ".Config ( overrides ) where"
    , ""
    , "import qualified Data.Text as T"
    , "import Data.Text (Text)"
    , ""
    , "-- | Overrides used when generating these bindings."
    , "overrides :: Text"
    , "overrides = T.unlines"
    , " [ " <> T.intercalate "\n , " (quoteOverrides maybeGiven) <> "]"
    ]

  where quoteOverrides :: Maybe TaggedOverride -> [Text]
        quoteOverrides Nothing = []
        quoteOverrides (Just (TaggedOverride _ ovText)) =
          map (T.pack . show) (T.lines ovText)

-- | A convenience helper for `confHook`, such that bindings for the
-- given module are generated in the @configure@ step of @cabal@.
confCodeGenHook :: Text -- ^ name
                -> Text -- ^ version
                -> Bool -- ^ verbose
                -> Maybe FilePath -- ^ overrides file
                -> [TaggedOverride] -- ^ other overrides
                -> Maybe FilePath -- ^ output dir
                -> ConfHook -- ^ previous `confHook`
                -> ConfHook
confCodeGenHook name version verbosity overridesFile inheritedOverrides outputDir
                defaultConfHook (gpd, hbi) flags = do

  givenOvs <- traverse (\fname -> TaggedOverride (T.pack fname) <$> utf8ReadFile fname) overridesFile

  let ovs = maybe inheritedOverrides (:inheritedOverrides) givenOvs
  m <- genModuleCode name version verbosity ovs

  let buildInfo = MN.fromString . T.unpack $ "GI." <> ucFirst name <> ".Config"
      em' = buildInfo : map (MN.fromString . T.unpack) (listModuleTree m)
      lib = ((condTreeData . fromJust . condLibrary) gpd)
      bi = libBuildInfo lib
#if MIN_VERSION_base(4,11,0)
      bi' = bi {autogenModules = em'}
#else
      bi' = bi
#endif
      lib' = lib {exposedModules = em', libBuildInfo = bi'}
      cL' = ((fromJust . condLibrary) gpd) {condTreeData = lib'}
      gpd' = gpd {condLibrary = Just cL'}

  void $ writeModuleTree verbosity outputDir m

  genConfigModule outputDir name givenOvs

  lbi <- defaultConfHook (gpd', hbi) flags

  return (lbi {withOptimization = NoOptimisation})

-- | The entry point for @Setup.hs@ files in bindings.
setupHaskellGIBinding :: Text -- ^ name
                      -> Text -- ^ version
                      -> Bool -- ^ verbose
                      -> Maybe FilePath -- ^ overrides file
                      -> Maybe FilePath -- ^ output dir
                      -> IO ()
setupHaskellGIBinding name version verbose overridesFile outputDir =
  setupBinding name version verbose overridesFile [] outputDir

-- | The entry point for @Setup.hs@ files in bindings.
setupBinding :: Text -- ^ name
             -> Text -- ^ version
             -> Bool -- ^ verbose
             -> Maybe FilePath -- ^ overrides file
             -> [TaggedOverride] -- ^ Explicit overrides
             -> Maybe FilePath -- ^ output dir
             -> IO ()
setupBinding name version verbose overridesFile overrides outputDir =
    defaultMainWithHooks (simpleUserHooks {
                            confHook = confCodeGenHook name version verbose
                                       overridesFile overrides outputDir
                                       (confHook simpleUserHooks)
                          })

-- | Return the list of modules that `setupHaskellGIBinding` would
-- create, together with the set of dependencies loaded while
-- generating the code.
configureDryRun :: Text -- ^ name
                -> Text -- ^ version
                -> Maybe FilePath -- ^ Overrides file
                -> [TaggedOverride] -- ^ Other overrides to load
                -> IO ([Text], S.Set Text)
configureDryRun name version overridesFile inheritedOverrides = do
  givenOvs <- traverse (\fname -> TaggedOverride (T.pack fname) <$> utf8ReadFile fname) overridesFile

  let ovs = maybe inheritedOverrides (:inheritedOverrides) givenOvs
  m <- genModuleCode name version False ovs

  return (("GI." <> ucFirst name <> ".Config") : listModuleTree m,
           transitiveModuleDeps m)
