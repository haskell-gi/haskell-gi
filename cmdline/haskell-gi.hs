module Main where

#if !MIN_VERSION_base(4,8,0)
import Data.Traversable (traverse)
#endif
import Control.Monad (forM_, when, (>=>))
import Control.Exception (handle)

import Data.Char (toLower)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)

import System.Directory (doesFileExist)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import System.FilePath (joinPath)
import System.IO (hPutStr, hPutStrLn, stderr)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as S

import Data.GI.Base.GError
import Text.Show.Pretty (ppShow)

import Data.GI.CodeGen.API (loadGIRInfo, loadRawGIRInfo, GIRInfo(girAPIs, girNSName), Name, API)
import Data.GI.CodeGen.Cabal (cabalConfig, setupHs, genCabalProject)
import Data.GI.CodeGen.Code (genCode, evalCodeGen, transitiveModuleDeps, writeModuleTree, writeModuleCode, moduleCode, codeToText, minBaseVersion)
import Data.GI.CodeGen.Config (Config(..))
import Data.GI.CodeGen.CodeGen (genModule)
import Data.GI.CodeGen.OverloadedLabels (genOverloadedLabels)
import Data.GI.CodeGen.OverloadedSignals (genOverloadedSignalConnectors)
import Data.GI.CodeGen.Overrides (Overrides, parseOverridesFile, nsChooseVersion, filterAPIsAndDeps, girFixups)
import Data.GI.CodeGen.ProjectInfo (licenseText)
import Data.GI.CodeGen.Util (ucFirst)

data Mode = GenerateCode | Dump | Labels | Signals | Help

data Options = Options {
  optMode :: Mode,
  optOutputDir :: Maybe String,
  optOverridesFiles :: [String],
  optSearchPaths :: [String],
  optVerbose :: Bool,
  optCabal :: Bool}

defaultOptions :: Options
defaultOptions = Options {
  optMode = GenerateCode,
  optOutputDir = Nothing,
  optOverridesFiles = [],
  optSearchPaths = [],
  optVerbose = False,
  optCabal = True}

parseKeyValue :: String -> (String, String)
parseKeyValue s =
  let (a, '=':b) = break (=='=') s
   in (a, b)

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
    (\arg opt -> opt { optSearchPaths = arg : optSearchPaths opt }) "PATH")
    "\tprepend a directory to the typelib search path",
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
  let version = M.lookup name (nsChooseVersion ovs)
  gir <- loadRawGIRInfo verbose name version extraPaths
  return (girAPIs gir)

-- | Generate overloaded labels ("_label", for example).
genLabels :: Options -> Overrides -> [Text] -> [FilePath] -> IO ()
genLabels options ovs modules extraPaths = do
  apis <- mapM (loadRawAPIs (optVerbose options) ovs extraPaths) modules
  let allAPIs = M.unions (map M.fromList apis)
      cfg = Config {modName = Nothing,
                    verbose = optVerbose options,
                    overrides = ovs}
  putStrLn $ "\t* Generating GI.OverloadedLabels"
  m <- genCode cfg allAPIs ["GI", "OverloadedLabels"]
       (genOverloadedLabels (M.toList allAPIs))
  _ <- writeModuleCode (optVerbose options) (optOutputDir options) m
  return ()

-- Generate generic signal connectors ("Clicked", "Activate", ...)
genGenericConnectors :: Options -> Overrides -> [Text] -> [FilePath] -> IO ()
genGenericConnectors options ovs modules extraPaths = do
  apis <- mapM (loadRawAPIs (optVerbose options) ovs extraPaths) modules
  let allAPIs = M.unions (map M.fromList apis)
      cfg = Config {modName = Nothing,
                    verbose = optVerbose options,
                    overrides = ovs}
  putStrLn $ "\t* Generating GI.Signals"
  m <- genCode cfg allAPIs ["GI", "Signals"] (genOverloadedSignalConnectors (M.toList allAPIs))
  _ <- writeModuleCode (optVerbose options) (optOutputDir options) m
  return ()

-- Generate the code for the given module, and return the dependencies
-- for this module.
processMod :: Options -> Overrides -> [FilePath] -> Text -> IO ()
processMod options ovs extraPaths name = do
  let version = M.lookup name (nsChooseVersion ovs)
      cfg = Config {modName = Just name,
                    verbose = optVerbose options,
                    overrides = ovs}
      nm = ucFirst name
      mp = T.unpack . ("GI." <>)

  putStrLn $ "\t* Generating " ++ mp nm

  (gir, girDeps) <- loadGIRInfo (optVerbose options) name version extraPaths
                    (girFixups ovs)
  let (apis, deps) = filterAPIsAndDeps ovs gir girDeps
      allAPIs = M.union apis deps

  m <- genCode cfg allAPIs ["GI", nm] (genModule apis)
  let modDeps = transitiveModuleDeps m
  moduleList <- writeModuleTree (optVerbose options) (optOutputDir options) m

  when (optCabal options) $ do
    let cabal = "gi-" ++ map toLower (T.unpack nm) ++ ".cabal"
    fname <- doesFileExist cabal >>=
             bool (return cabal)
                  (putStrLn (cabal ++ " exists, writing "
                             ++ cabal ++ ".new instead") >>
                   return (cabal ++ ".new"))
    -- The module is not a dep of itself
    let usedDeps = S.delete name modDeps
        -- We only list as dependencies in the cabal file the
        -- dependencies that we use, disregarding what the .gir file says.
        actualDeps = filter ((`S.member` usedDeps) . girNSName) girDeps
        baseVersion = minBaseVersion m
        p = \n -> joinPath [fromMaybe "" (optOutputDir options), n]
    (err, m) <- evalCodeGen cfg allAPIs [] (genCabalProject gir actualDeps moduleList baseVersion)
    case err of
      Nothing -> do
               putStrLn $ "\t\t+ " ++ fname
               TIO.writeFile (p fname) (codeToText (moduleCode m))
               putStrLn "\t\t+ cabal.config"
               TIO.writeFile (p "cabal.config") cabalConfig
               putStrLn "\t\t+ Setup.hs"
               TIO.writeFile (p "Setup.hs") setupHs
               putStrLn "\t\t+ LICENSE"
               TIO.writeFile (p "LICENSE") licenseText
      Just msg -> putStrLn $ "ERROR: could not generate " ++ fname
                  ++ "\nError was: " ++ T.unpack msg

dump :: Options -> Overrides -> Text -> IO ()
dump options ovs name = do
  let version = M.lookup name (nsChooseVersion ovs)
  (doc, _) <- loadGIRInfo (optVerbose options) name version (optSearchPaths options) []
  mapM_ (putStrLn . ppShow) (girAPIs doc)

process :: Options -> [Text] -> IO ()
process options names = do
  let extraPaths = optSearchPaths options
  configs <- traverse TIO.readFile (optOverridesFiles options)
  parseOverridesFile (concatMap T.lines configs) >>= \case
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
