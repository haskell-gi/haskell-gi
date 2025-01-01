-- | Convenience hooks for writing custom @Setup.hs@ files for
-- bindings.
module Data.GI.CodeGen.CabalHooks
    ( setupBinding
    , setupCompatWrapper
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
import Data.GI.CodeGen.Util (utf8ReadFile, utf8WriteFile, ucFirst, splitOn)

import System.Directory (createDirectoryIfMissing)
import System.FilePath (joinPath, takeDirectory, (</>))

import Control.Monad (forM)

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
              -> Text -- ^ pkgName
              -> Text -- ^ pkgVersion
              -> Bool -- ^ verbose
              -> [TaggedOverride] -- ^ Explicit overrides
              -> IO ModuleInfo
genModuleCode name version pkgName pkgVersion verbosity overrides = do
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
                    modVersion = version,
                    ghcPkgName = pkgName,
                    ghcPkgVersion = pkgVersion,
                    verbose = verbosity,
                    overrides = ovs}

  return $ genCode cfg allAPIs (toModulePath name) (genModule apis)

-- | Write a module containing information about the configuration for
-- the package.
genConfigModule :: Maybe FilePath -> Text -> Maybe TaggedOverride ->
                   [Text] -> IO ()
genConfigModule outputDir modName maybeGiven modules = do
  let fname = joinPath [ fromMaybe "" outputDir
                       , "GI"
                       , T.unpack (ucFirst modName)
                       , "Config.hs" ]
      dirname = takeDirectory fname

  createDirectoryIfMissing True dirname

  utf8WriteFile fname $ T.unlines
    [ "{-# LANGUAGE OverloadedStrings #-}"
    , "-- | Build time configuration used during code generation."
    , "module GI." <> ucFirst modName <> ".Config ( overrides, modules ) where"
    , ""
    , "import qualified Data.Text as T"
    , "import Data.Text (Text)"
    , ""
    , "-- | Overrides used when generating these bindings."
    , "overrides :: Text"
    , "overrides = T.unlines"
    , formatList (overrides maybeGiven)
    , ""
    , "-- | Modules in this package"
    , "modules :: [Text]"
    , "modules = " <>
      formatList (("GI." <> ucFirst modName <> ".Config") : modules)
    ]

  where overrides :: Maybe TaggedOverride -> [Text]
        overrides Nothing = []
        overrides (Just (TaggedOverride _ ovText)) = T.lines ovText

        formatList :: [Text] -> Text
        formatList l = " [ "
                       <> T.intercalate "\n , " (map (T.pack . show) l)
                       <> "]"

-- | A convenience helper for `confHook`, such that bindings for the
-- given module are generated in the @configure@ step of @cabal@.
confCodeGenHook :: Text -- ^ name
                -> Text -- ^ version
                -> Text -- ^ pkgName
                -> Text -- ^ pkgVersion
                -> Bool -- ^ verbose
                -> Maybe FilePath -- ^ overrides file
                -> [TaggedOverride] -- ^ other overrides
                -> Maybe FilePath -- ^ output dir
                -> ConfHook -- ^ previous `confHook`
                -> ConfHook
confCodeGenHook name version pkgName pkgVersion verbosity
                overridesFile inheritedOverrides outputDir
                defaultConfHook (gpd, hbi) flags = do

  givenOvs <- traverse (\fname -> TaggedOverride (T.pack fname) <$> utf8ReadFile fname) overridesFile

  let ovs = maybe inheritedOverrides (:inheritedOverrides) givenOvs
  m <- genModuleCode name version pkgName pkgVersion verbosity ovs

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

  modules <- writeModuleTree verbosity outputDir m

  genConfigModule outputDir name givenOvs modules

  lbi <- defaultConfHook (gpd', hbi) flags

  return (lbi {withOptimization = NoOptimisation})

-- | The entry point for @Setup.hs@ files in bindings.
setupBinding :: Text -- ^ name
             -> Text -- ^ version
             -> Text -- ^ pkgName
             -> Text -- ^ pkgVersion
             -> Bool -- ^ verbose
             -> Maybe FilePath -- ^ overrides file
             -> [TaggedOverride] -- ^ Explicit overrides
             -> Maybe FilePath -- ^ output dir
             -> IO ()
setupBinding name version pkgName pkgVersion verbose overridesFile overrides outputDir =
    defaultMainWithHooks (simpleUserHooks {
                            confHook = confCodeGenHook name version
                                       pkgName pkgVersion
                                       verbose
                                       overridesFile overrides outputDir
                                       (confHook simpleUserHooks)
                          })

compatGenConfHook :: String -- ^ New version of the package
                  -> [Text] -- ^ The list of modules to re-export
                  -> ConfHook -- ^ previous `confHook`
                  -> ConfHook
compatGenConfHook newVersion modules defaultConfHook (gpd, hbi) flags = do
  let em' = map (MN.fromString . T.unpack) modules
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

  mapM_ (writeCompatModule . T.unpack) modules

  lbi <- defaultConfHook (gpd', hbi) flags

  return (lbi {withOptimization = NoOptimisation})

  where
    writeCompatModule :: String -> IO ()
    writeCompatModule modName = do
      fname <- case unsnoc (splitOn '.' modName) of
                 Nothing -> return $ modName <> ".hs"
                 Just ([], last) -> return $ last <> ".hs"
                 Just (init, last) -> let path = joinPath init
                                      in do
                                        createDirectoryIfMissing True path
                                        return $ path </> (last <> ".hs")
      utf8WriteFile fname modContents

      where modContents :: Text
            modContents = let
              mod = T.pack modName
              link = "[" <> T.pack newVersion
                     <> "](https://hackage.haskell.org/package/"
                     <> T.pack newVersion <> ")"
              in T.unlines [
              "{-# LANGUAGE PackageImports #-}"
              , "{- | This is a backwards-compatibility module re-exporting the contents of the "
              , mod <> " module in the " <> link <> " package."
              , ""
              , "The link below will take you to the relevant entry in the " <> link <> " documentation."
              , "-}"
              , "module " <> mod <> " ("
              , "  module X) where"
              , ""
              , "import \"" <> T.pack newVersion <> "\" " <> mod <> " as X"
              ]

            -- Data.List.unsnoc is relatively recent (base 4.19.0.0),
            -- so we just copy the definition.
            unsnoc :: [a] -> Maybe ([a], a)
            unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing

-- | The entry point for @Setup.hs@ files in compat bindings.
setupCompatWrapper :: String   -- ^ New package
                   -> [Text] -- ^ List of files in the new package
                   -> IO ()
setupCompatWrapper newPackage modules =
    defaultMainWithHooks (simpleUserHooks {
                            confHook = compatGenConfHook newPackage modules
                                       (confHook simpleUserHooks)
                          })

-- | Return the list of modules that `setupHaskellGIBinding` would
-- create, together with the set of dependencies loaded while
-- generating the code.
configureDryRun :: Text -- ^ name
                -> Text -- ^ version
                -> Text -- ^ pkgName
                -> Text -- ^ pkgVersion
                -> Maybe FilePath -- ^ Overrides file
                -> [TaggedOverride] -- ^ Other overrides to load
                -> IO ([Text], S.Set Text)
configureDryRun name version pkgName pkgVersion overridesFile inheritedOverrides = do
  givenOvs <- traverse (\fname -> TaggedOverride (T.pack fname) <$> utf8ReadFile fname) overridesFile

  let ovs = maybe inheritedOverrides (:inheritedOverrides) givenOvs
  m <- genModuleCode name version pkgName pkgVersion False ovs

  return (("GI." <> ucFirst name <> ".Config") : listModuleTree m,
           transitiveModuleDeps m)
