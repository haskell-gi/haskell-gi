module Main where

#if !MIN_VERSION_base(4,8,0)
import Data.Traversable (traverse)
#endif
import Control.Monad (forM_, forM, when, (>=>))
import Control.Exception (handle)
import Control.Applicative ((<|>))

import Data.Char (toLower)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import System.Directory (doesFileExist)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import System.FilePath (joinPath)
import System.IO (hPutStr, hPutStrLn, stderr)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Set as S

import Data.GI.Base.GError
import Text.Show.Pretty (ppShow)

import Data.GI.CodeGen.API (loadGIRInfo, loadRawGIRInfo, GIRInfo(girAPIs, girNSName), Name, API)
import Data.GI.CodeGen.Cabal (cabalConfig, setupHs, genCabalProject, tryPkgConfig)
import Data.GI.CodeGen.Code (genCode, transitiveModuleDeps, writeModuleTree, moduleCode, codeToText, minBaseVersion)
import Data.GI.CodeGen.Config (Config(..))
import Data.GI.CodeGen.CodeGen (genModule)
import Data.GI.CodeGen.LibGIRepository (setupTypelibSearchPath)
import Data.GI.CodeGen.ModulePath (toModulePath)
import Data.GI.CodeGen.OverloadedLabels (genOverloadedLabels)
import Data.GI.CodeGen.OverloadedSignals (genOverloadedSignalConnectors)
import Data.GI.CodeGen.Overrides (Overrides, parseOverrides, nsChooseVersion, filterAPIsAndDeps, girFixups, pkgConfigMap)
import Data.GI.CodeGen.ProjectInfo (licenseText)
import Data.GI.CodeGen.Util (ucFirst, utf8WriteFile, utf8ReadFile, terror)

data Mode = GenerateCode | Dump | Labels | Signals | Help

data Options = Options {
  optMode :: Mode,
  optOutputDir :: Maybe String,
  optOverridesFiles :: [String],
  optSearchPaths :: [String],
  optVerbose :: Bool,
  optCabal :: Bool,
  optOvMethods :: Bool,
  optOvProperties :: Bool,
  optOvSignals :: Bool
}

defaultOptions :: Options
defaultOptions = Options {
  optMode = GenerateCode,
  optOutputDir = Nothing,
  optOverridesFiles = [],
  optSearchPaths = [],
  optVerbose = False,
  optCabal = True,
  optOvMethods = True,
  optOvProperties = True,
  optOvSignals = True
}

parseKeyValue :: String -> (String, String)
parseKeyValue s =
  let (a, '=':b) = break (=='=') s
   in (a, b)

solveNameVersion::Overrides -> Text -> (Text, Maybe Text)
solveNameVersion ovs name = (nameWithoutVersion, version)
 where
   namePieces = T.splitOn "-" name
   nameWithoutVersion = head namePieces
   versionInName = if length namePieces == 2
                     then Just (last namePieces)
                     else Nothing
   versionInOverride = M.lookup nameWithoutVersion (nsChooseVersion ovs)
   version = versionInOverride <|> versionInName

optDescrs :: [OptDescr (Options -> Options)]
optDescrs = [
  Option "h" ["help"] (NoArg $ \opt -> opt { optMode = Help })
    "\tprint this gentle help text",
  Option "c" ["connectors"] (NoArg $ \opt -> opt {optMode = Signals})
    "generate generic signal connectors",
  Option "d" ["dump"] (NoArg $ \opt -> opt { optMode = Dump })
    "\tdump internal representation",
  Option "l" ["labels"] (NoArg $ \opt -> opt {optMode = Labels})
    "\tgenerate overloaded labels",
  Option "n" ["no-cabal"] (NoArg $ \opt -> opt {optCabal = False})
    ("\tdo not generate cabal project structure\n" ++
     "\t\t\t(note: if you do not generate a cabal project, make sure\n" ++
     "\t\t\tto still activate the necessary GHC extensions when\n" ++
     "\t\t\tcompiling the generated bindings.)"),
  Option "o" ["overrides"] (ReqArg
                           (\arg opt -> opt {optOverridesFiles =
                                                 arg : optOverridesFiles opt})
                          "OVERRIDES")
    "specify a file with overrides info",
  Option "O" ["output"] (ReqArg
                         (\arg opt -> opt {optOutputDir = Just arg}) "DIR")
    "\tset the output directory",
  Option "s" ["search"] (ReqArg
    (\arg opt -> opt { optSearchPaths = arg : optSearchPaths opt}) "PATH")
    "\tprepend a directory to the GIR and typelib search path",
  Option "M" ["noMethodOverloading"] (NoArg $ \opt -> opt {optOvMethods = False})
    "\tdo not generate method overloading support",
  Option "P" ["noPropertyOverloading"] (NoArg $ \opt -> opt {optOvProperties = False})
    "\tdo not generate property overloading support",
  Option "S" ["noSignalOverloading"] (NoArg $ \opt -> opt {optOvSignals = False})
    "\tdo not generate signal overloading support",
  Option "v" ["verbose"] (NoArg $ \opt -> opt { optVerbose = True })
    "\tprint extra info while processing"]

showHelp :: String
showHelp = concatMap optAsLine optDescrs
  where optAsLine (Option flag (long:_) _ desc) =
          "  -" ++ flag ++ "|--" ++ long ++ "\t" ++ desc ++ "\n"
        optAsLine _ = error "showHelp"

printGError :: IO () -> IO ()
printGError = handle (gerrorMessage >=> putStrLn . T.unpack)

-- | Load a dependency without further postprocessing.
loadRawAPIs :: Bool -> Overrides -> [FilePath] -> Text -> IO [(Name, API)]
loadRawAPIs verbose ovs extraPaths name = do
  let (nameWithoutVersion, version) = solveNameVersion ovs name
  gir <- loadRawGIRInfo verbose nameWithoutVersion version extraPaths
  return (girAPIs gir)

-- | Generate overloaded labels ("_label", for example).
genLabels :: Options -> Overrides -> [Text] -> [FilePath] -> IO ()
genLabels options ovs modules extraPaths = do
  apis <- mapM (loadRawAPIs (optVerbose options) ovs extraPaths) modules
  let allAPIs = M.unions (map M.fromList apis)
      cfg = Config {modName = "<<GI autogenerated labels>>",
                    verbose = optVerbose options,
                    overrides = ovs
                   }
  putStrLn $ "\t* Generating GI.OverloadedLabels"
  let m = genCode cfg allAPIs  "OverloadedLabels"
        (genOverloadedLabels (M.toList allAPIs))
  _ <- writeModuleTree (optVerbose options) (optOutputDir options) m
  return ()

-- | Generate generic signal connectors ("Clicked", "Activate", ...)
genGenericConnectors :: Options -> Overrides -> [Text] -> [FilePath] -> IO ()
genGenericConnectors options ovs modules extraPaths = do
  apis <- mapM (loadRawAPIs (optVerbose options) ovs extraPaths) modules
  let allAPIs = M.unions (map M.fromList apis)
      cfg = Config {modName = "<<GI autogenerated connectors>>",
                    verbose = optVerbose options,
                    overrides = ovs
                   }
  putStrLn $ "\t* Generating GI.Signals"
  let m = genCode cfg allAPIs "Signals" (genOverloadedSignalConnectors (M.toList allAPIs))
  _ <- writeModuleTree (optVerbose options) (optOutputDir options) m
  return ()

-- | Generate the code for the given module, and return the dependencies
-- for this module.
processMod :: Options -> Overrides -> [FilePath] -> Text -> IO ()
processMod options ovs extraPaths name = do
  let (nameWithoutVersion, version) = solveNameVersion ovs name
      nm = ucFirst nameWithoutVersion
      mp = T.unpack . ("GI." <>)

  putStrLn $ "\t* Generating " ++ mp nm

  (gir, girDeps) <- loadGIRInfo (optVerbose options) nameWithoutVersion version extraPaths
                    (girFixups ovs)
  let (apis, deps) = filterAPIsAndDeps ovs gir girDeps
      allAPIs = M.union apis deps
      cfg = Config {modName = nameWithoutVersion,
                    verbose = optVerbose options,
                    overrides = ovs}
      m = genCode cfg allAPIs (toModulePath nm) (genModule apis)
      modDeps = transitiveModuleDeps m

  moduleList <- writeModuleTree (optVerbose options) (optOutputDir options) m

  when (optCabal options) $ do
    let cabal = "gi-" ++ map toLower (T.unpack nm) ++ ".cabal"
    fname <- doesFileExist cabal >>=
             bool (return cabal)
                  (putStrLn (cabal ++ " exists, writing "
                             ++ cabal ++ ".new instead") >>
                   return (cabal ++ ".new"))

    let pkMap = pkgConfigMap (overrides cfg)

    pkgInfo <- tryPkgConfig gir (verbose cfg) pkMap >>= \case
        Left err -> terror err
        Right info -> return info

    -- The module is not a dep of itself
    let usedDeps = S.delete nameWithoutVersion modDeps
        -- We only list as dependencies in the cabal file the
        -- dependencies that we use, disregarding what the .gir file says.
        actualDeps = filter ((`S.member` usedDeps) . girNSName) girDeps
        baseVersion = minBaseVersion m
        p = \n -> joinPath [fromMaybe "" (optOutputDir options), n]

    resolvedDeps <- forM actualDeps $ \dep -> do
      tryPkgConfig dep (verbose cfg) pkMap >>= \case
        Left err -> terror err
        Right info -> return (dep, info)

    let m = genCode cfg allAPIs (error "undefined module path")
            (genCabalProject (gir, pkgInfo) resolvedDeps moduleList baseVersion)

    putStrLn $ "\t\t+ " ++ fname
    utf8WriteFile (p fname) (codeToText (moduleCode m))
    putStrLn "\t\t+ cabal.config"
    utf8WriteFile (p "cabal.config") cabalConfig
    putStrLn "\t\t+ Setup.hs"
    utf8WriteFile (p "Setup.hs") setupHs
    putStrLn "\t\t+ LICENSE"
    utf8WriteFile (p "LICENSE") (licenseText name)

dump :: Options -> Overrides -> Text -> IO ()
dump options ovs name = do
  let (nameWithoutVersion, version) = solveNameVersion ovs name
  (doc, _) <- loadGIRInfo (optVerbose options) nameWithoutVersion version (optSearchPaths options) []
  mapM_ (putStrLn . ppShow) (girAPIs doc)

process :: Options -> [Text] -> IO ()
process options names = do
  let extraPaths = optSearchPaths options
  setupTypelibSearchPath (optSearchPaths options)
  configs <- traverse (utf8ReadFile >=> parseOverrides) (optOverridesFiles options)
  case mconcat <$> sequence configs of
    Left errorMsg -> do
      hPutStr stderr "Error when parsing the config file(s):\n"
      hPutStr stderr (T.unpack errorMsg)
      exitFailure
    Right ovs ->
      case optMode options of
        GenerateCode -> forM_ names (processMod options ovs extraPaths)
        Labels -> genLabels options ovs names extraPaths
        Signals -> genGenericConnectors options ovs names extraPaths
        Dump -> forM_ names (dump options ovs)
        Help -> putStr showHelp

main :: IO ()
main = printGError $ do
    args <- getArgs
    let (actions, nonOptions, errors) = getOpt RequireOrder optDescrs args
        options  = foldl (.) id actions defaultOptions

    case errors of
        [] -> return ()
        _ -> do
            mapM_ (hPutStr stderr) errors
            exitFailure

    case nonOptions of
      [] -> failWithUsage
      names -> process options (map T.pack names)
    where
      failWithUsage = do
        hPutStrLn stderr "usage: haskell-gi [options] module1 [module2 [...]]"
        hPutStr stderr showHelp
        exitFailure
