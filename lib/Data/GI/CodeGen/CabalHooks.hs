-- | Convenience hooks for writing custom @Setup.hs@ files for
-- bindings.
module Data.GI.CodeGen.CabalHooks
    ( setupHaskellGIBinding
    ) where

import qualified Distribution.ModuleName as MN
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple (UserHooks(..), simpleUserHooks,
                            defaultMainWithHooks, OptimisationLevel(..),
                            Dependency(..), PackageName(..))
import Distribution.PackageDescription

import Data.GI.CodeGen.API (loadGIRInfo)
import Data.GI.CodeGen.Code (genCode, writeModuleTree, listModuleTree)
import Data.GI.CodeGen.CodeGen (genModule)
import Data.GI.CodeGen.Config (Config(..), CodeGenFlags(..))
import Data.GI.CodeGen.LibGIRepository (setupTypelibSearchPath)
import Data.GI.CodeGen.ModulePath (toModulePath)
import Data.GI.CodeGen.Overrides (parseOverridesFile, girFixups,
                                  filterAPIsAndDeps)
import Data.GI.CodeGen.PkgConfig (tryPkgConfig)
import Data.GI.CodeGen.Util (ucFirst, tshow, utf8ReadFile, utf8WriteFile)

import Control.Monad (when)

import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import System.Directory (doesFileExist)
import System.FilePath ((</>), (<.>))

type ConfHook = (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags
              -> IO LocalBuildInfo

-- | Generate the @PkgInfo@ module, listing the build information for
-- the module. We include in particular the versions for the
-- `pkg-config` dependencies of the module.
genPkgInfo :: [Dependency] -> [(FlagName, Bool)] -> FilePath -> Text -> IO ()
genPkgInfo deps flags fName modName = do
  versions <- mapM findVersion deps
  utf8WriteFile fName $ T.unlines
         [ "module " <> modName <> " (pkgConfigVersions, flags) where"
         , ""
         , "import Prelude (String, Bool(..))"
         , ""
         , "pkgConfigVersions :: [(String, String)]"
         , "pkgConfigVersions = " <> tshow versions
         , ""
         , "flags :: [(String, Bool)]"
         , "flags = " <> tshow flags'
         ]
    where findVersion :: Dependency -> IO (Text, Text)
          findVersion (Dependency (PackageName n) _) =
              tryPkgConfig (T.pack n) >>= \case
                  Just v -> return v
                  Nothing -> error ("Could not determine version for required pkg-config module \"" <> n <> "\".")

          flags' :: [(String, Bool)]
          flags' = map (\(FlagName fn, v) -> (fn, v)) flags

-- | Parse the set of flags given to configure into flags for the code
-- generator.
parseFlags :: [(FlagName, Bool)] -> CodeGenFlags
parseFlags fs = parsed
    where parsed :: CodeGenFlags
          parsed = CodeGenFlags {
                    cgOverloadedProperties = check "overloaded-properties"
                  , cgOverloadedSignals = check "overloaded-signals"
                  , cgOverloadedMethods = check "overloaded-methods"
                  }

          check :: String -> Bool
          check s = fromMaybe True (M.lookup s flags)

          flags :: M.Map String Bool
          flags = M.fromList (map (\(FlagName fn, v) -> (fn, v)) fs)

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
  setupTypelibSearchPath []

  ovsData <- case overrides of
               Nothing -> return ""
               Just fname -> utf8ReadFile fname
  ovs <- parseOverridesFile (T.lines ovsData) >>= \case
         Left err -> error $ "Error when parsing overrides file: "
                     ++ T.unpack err
         Right ovs -> return ovs

  (gir, girDeps) <- loadGIRInfo verbosity name (Just version) [] (girFixups ovs)
  let (apis, deps) = filterAPIsAndDeps ovs gir girDeps
      allAPIs = M.union apis deps
      cfg = Config {modName = Just name,
                    verbose = verbosity,
                    overrides = ovs,
                    cgFlags = parseFlags (configConfigurationsFlags flags)}

  m <- genCode cfg allAPIs (toModulePath name) (genModule apis)
  alreadyDone <- doesFileExist (fromMaybe "" outputDir
                                </> "GI" </> T.unpack (ucFirst name) <.> "hs")
  moduleList <- if not alreadyDone
                then writeModuleTree verbosity outputDir m
                else return (listModuleTree m)

  let pkgInfoMod = "GI." <> ucFirst name <> ".PkgInfo"
      em' = map (MN.fromString . T.unpack) (pkgInfoMod : moduleList)
      ctd' = ((condTreeData . fromJust . condLibrary) gpd) {exposedModules = em'}
      cL' = ((fromJust . condLibrary) gpd) {condTreeData = ctd'}
      gpd' = gpd {condLibrary = Just cL'}

  when (not alreadyDone) $
       genPkgInfo ((pkgconfigDepends . libBuildInfo . condTreeData .
                    fromJust . condLibrary) gpd)
                  (configConfigurationsFlags flags)
                  (fromMaybe "" outputDir
                   </> "GI" </> T.unpack (ucFirst name) </> "PkgInfo.hs")
                  pkgInfoMod

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
