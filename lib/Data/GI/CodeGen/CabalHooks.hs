-- | Convenience hooks for writing custom @Setup.hs@ files for
-- bindings.
module Data.GI.CodeGen.CabalHooks
    ( confCodeGenHook
    ) where

import qualified Distribution.ModuleName as MN
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.PackageDescription

import Data.GI.CodeGen.API (loadGIRInfo)
import Data.GI.CodeGen.Code (genCode, writeModuleTree)
import Data.GI.CodeGen.CodeGen (genModule)
import Data.GI.CodeGen.Config (Config(..))
import Data.GI.CodeGen.Overrides (parseOverridesFile, girFixups,
                                  filterAPIsAndDeps)
import Data.GI.CodeGen.Util (ucFirst)

import Data.Maybe (fromJust)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type ConfHook = (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags
              -> IO LocalBuildInfo

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
  ovsData <- case overrides of
               Nothing -> return ""
               Just fname -> TIO.readFile fname
  ovs <- parseOverridesFile (T.lines ovsData) >>= \case
         Left err -> error $ "Error when parsing overrides file: "
                     ++ T.unpack err
         Right ovs -> return ovs

  (gir, girDeps) <- loadGIRInfo verbosity name (Just version) [] (girFixups ovs)
  let (apis, deps) = filterAPIsAndDeps ovs gir girDeps
      allAPIs = M.union apis deps
      cfg = Config {modName = Just name,
                    verbose = verbosity,
                    overrides = ovs}

  m <- genCode cfg allAPIs ["GI", ucFirst name] (genModule apis)
  moduleList <- writeModuleTree verbosity outputDir m

  let em' = map (MN.fromString . T.unpack) moduleList
      ctd' = ((condTreeData . fromJust . condLibrary) gpd) {exposedModules = em'}
      cL' = ((fromJust . condLibrary) gpd) {condTreeData = ctd'}
      gpd' = gpd {condLibrary = Just cL'}

  defaultConfHook (gpd', hbi) flags
