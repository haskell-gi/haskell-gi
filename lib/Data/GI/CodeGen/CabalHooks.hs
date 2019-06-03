-- | Convenience hooks for writing custom @Setup.hs@ files for
-- bindings.
module Data.GI.CodeGen.CabalHooks
    ( setupHaskellGIBinding
    , configureDryRun
    ) where

import qualified Distribution.ModuleName as MN
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple (UserHooks(..), simpleUserHooks,
                            defaultMainWithHooks, OptimisationLevel(..))
import Distribution.PackageDescription

import Data.GI.CodeGen.API (loadGIRInfo)
import Data.GI.CodeGen.Code (genCode, writeModuleTree, listModuleTree,
                             ModuleInfo)
import Data.GI.CodeGen.CodeGen (genModule)
import Data.GI.CodeGen.Config (Config(..))
import Data.GI.CodeGen.LibGIRepository (setupTypelibSearchPath)
import Data.GI.CodeGen.ModulePath (toModulePath)
import Data.GI.CodeGen.Overrides (parseOverridesFile, girFixups,
                                  filterAPIsAndDeps)

import Control.Monad (void)

import Data.Maybe (fromJust)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T

type ConfHook = (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags
              -> IO LocalBuildInfo

-- | Generate the code for the given module.
genModuleCode :: Text -- ^ name
              -> Text -- ^ version
              -> Bool -- ^ verbose
              -> Maybe FilePath -- ^ overrides file
              -> IO ModuleInfo
genModuleCode name version verbosity overrides = do
  setupTypelibSearchPath []

  ovs <- case overrides of
    Nothing -> return mempty
    Just fname -> parseOverridesFile fname >>= \case
         Left err -> error $ "Error when parsing overrides file: "
                     ++ T.unpack err
         Right ovs -> return ovs

  (gir, girDeps) <- loadGIRInfo verbosity name (Just version) [] (girFixups ovs)
  let (apis, deps) = filterAPIsAndDeps ovs gir girDeps
      allAPIs = M.union apis deps
      cfg = Config {modName = name,
                    verbose = verbosity,
                    overrides = ovs}

  return $ genCode cfg allAPIs (toModulePath name) (genModule apis)

-- | A convenience helper for `confHook`, such that bindings for the
-- given module are generated in the @configure@ step of @cabal@.
confCodeGenHook :: Text -- ^ name
                -> Text -- ^ version
                -> Bool -- ^ verbose
                -> Maybe FilePath -- ^ overrides file
                -> Maybe FilePath -- ^ output dir
                -> ConfHook -- ^ previous `confHook`
                -> ConfHook
confCodeGenHook name version verbosity overrides outputDir
                defaultConfHook (gpd, hbi) flags = do
  m <- genModuleCode name version verbosity overrides

  let em' = map (MN.fromString . T.unpack) (listModuleTree m)
      ctd' = ((condTreeData . fromJust . condLibrary) gpd) {exposedModules = em'}
      cL' = ((fromJust . condLibrary) gpd) {condTreeData = ctd'}
      gpd' = gpd {condLibrary = Just cL'}

  void $ writeModuleTree verbosity outputDir m

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
    defaultMainWithHooks (simpleUserHooks {
                            confHook = confCodeGenHook name version verbose
                                       overridesFile outputDir
                                       (confHook simpleUserHooks)
                          })

-- | Return the list of modules that `setupHaskellGIBinding` would create.
configureDryRun :: Text -- ^ name
                -> Text -- ^ version
                -> Maybe FilePath
                -> IO [Text]
configureDryRun name version overrides = do
  m <- genModuleCode name version False overrides

  return (listModuleTree m)
