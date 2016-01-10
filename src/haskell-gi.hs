module Main where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Traversable (traverse)
#endif
import Control.Monad (forM_, when, (>=>))
import Control.Exception (handle)

import Data.Char (toLower)
import Data.Bool (bool)
import Data.List (intercalate)
import Data.Text (pack, unpack, Text)

import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (splitDirectories, joinPath)
import System.Console.GetOpt
import System.Exit
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Environment (getArgs)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as S

import Data.GI.Base.GError
import Text.Show.Pretty (ppShow)

import GI.API (loadGIRInfo, loadRawGIRInfo, GIRInfo(girAPIs, girNSName), Name, API)
import GI.Cabal (cabalConfig, setupHs, genCabalProject)
import GI.Code (codeToText, genCode, evalCodeGen, moduleCode, moduleDeps)
import GI.Config (Config(..))
import GI.CodeGen (genModule)
import GI.Attributes (genAttributes, genAllAttributes)
import GI.OverloadedSignals (genSignalInstances, genOverloadedSignalConnectors)
import GI.Overrides (Overrides, parseOverridesFile, nsChooseVersion, filterAPIsAndDeps)
import GI.ProjectInfo (licenseText)
import GI.SymbolNaming (ucFirst)

data Mode = GenerateCode | Dump | Attributes | Signals | Help

data Options = Options {
  optMode :: Mode,
  optOutput :: Maybe String,
  optOverridesFiles :: [String],
  optSearchPaths :: [String],
  optVerbose :: Bool,
  optCabal :: Bool}

defaultOptions = Options {
  optMode = GenerateCode,
  optOutput = Just "GI",
  optOverridesFiles = [],
  optSearchPaths = [],
  optVerbose = False,
  optCabal = True}

parseKeyValue s =
  let (a, '=':b) = break (=='=') s
   in (a, b)

optDescrs :: [OptDescr (Options -> Options)]
optDescrs = [
  Option "h" ["help"] (NoArg $ \opt -> opt { optMode = Help })
    "\tprint this gentle help text",
  Option "a" ["attributes"] (NoArg $ \opt -> opt {optMode = Attributes})
    "generate generic attribute accesors",
  Option "c" ["connectors"] (NoArg $ \opt -> opt {optMode = Signals})
    "generate generic signal connectors",
  Option "d" ["dump"] (NoArg $ \opt -> opt { optMode = Dump })
    "\tdump internal representation",
  Option "n" ["no-cabal"] (NoArg $ \opt -> opt {optCabal = False})
    "\tdo not generate .cabal file",
  Option "o" ["overrides"] (ReqArg
                           (\arg opt -> opt {optOverridesFiles =
                                                 arg : optOverridesFiles opt})
                          "OVERRIDES")
    "specify a file with overrides info",
  Option "O" ["output"] (ReqArg
                         (\arg opt -> opt {optOutput = Just arg}) "DIR")
    "\tset the output directory",
  Option "s" ["search"] (ReqArg
    (\arg opt -> opt { optSearchPaths = arg : optSearchPaths opt }) "PATH")
    "\tprepend a directory to the typelib search path",
  Option "v" ["verbose"] (NoArg $ \opt -> opt { optVerbose = True })
    "\tprint extra info while processing"]

showHelp = concatMap optAsLine optDescrs
  where optAsLine (Option flag (long:_) _ desc) =
          "  -" ++ flag ++ "|--" ++ long ++ "\t" ++ desc ++ "\n"
        optAsLine _ = error "showHelp"

printGError = handle (gerrorMessage >=> putStrLn . unpack)

outputPath :: Options -> IO (String, String) -- modPrefix, dirPrefix
outputPath options =
    case optOutput options of
      Nothing -> return ("", ".")
      Just dir -> do
        createDirectoryIfMissing True dir
        let prefix = intercalate "." (splitDirectories dir) ++ "."
        return (prefix, dir)

-- | Load a dependency without further postprocessing.
loadRawAPIs :: Bool -> Overrides -> [FilePath] -> Text -> IO [(Name, API)]
loadRawAPIs verbose ovs extraPaths name = do
  let version = M.lookup name (nsChooseVersion ovs)
  gir <- loadRawGIRInfo verbose name version extraPaths
  return (girAPIs gir)

-- Generate all generic accessor functions ("_label", for example).
genGenericAttrs :: Options -> Overrides -> [Text] -> [FilePath] -> IO ()
genGenericAttrs options ovs modules extraPaths = do
  apis <- mapM (loadRawAPIs (optVerbose options) ovs extraPaths) modules
  let allAPIs = M.unions (map M.fromList apis)
      cfg = Config {modName = Nothing,
                    verbose = optVerbose options,
                    overrides = ovs}
  (modPrefix, dirPrefix) <- outputPath options
  putStrLn $ "\t* Generating " ++ modPrefix ++ "Properties"
  m <- genCode cfg allAPIs ["XXX"] (genAllAttributes (M.toList allAPIs) modPrefix)
  TIO.writeFile (joinPath [dirPrefix, "Properties.hs"]) $
     codeToText (moduleCode m)

-- Generate generic signal connectors ("Clicked", "Activate", ...)
genGenericConnectors :: Options -> Overrides -> [Text] -> [FilePath] -> IO ()
genGenericConnectors options ovs modules extraPaths = do
  apis <- mapM (loadRawAPIs (optVerbose options) ovs extraPaths) modules
  let allAPIs = M.unions (map M.fromList apis)
      cfg = Config {modName = Nothing,
                    verbose = optVerbose options,
                    overrides = ovs}
  (modPrefix, dirPrefix) <- outputPath options
  putStrLn $ "\t* Generating " ++ modPrefix ++ "Signals"
  m <- genCode cfg allAPIs ["XXX"] (genOverloadedSignalConnectors (M.toList allAPIs) modPrefix)
  TIO.writeFile (joinPath [dirPrefix, "Signals.hs"]) $
     codeToText (moduleCode m)

-- Generate the code for the given module, and return the dependencies
-- for this module.
processMod :: Options -> Overrides -> [FilePath] -> String -> IO ()
processMod options ovs extraPaths name = do
  let version = M.lookup (T.pack name) (nsChooseVersion ovs)
  (gir, girDeps) <- loadGIRInfo (optVerbose options) (T.pack name) version extraPaths
  let (apis, deps) = filterAPIsAndDeps ovs gir girDeps
      allAPIs = M.union apis deps

  let cfg = Config {modName = Just name,
                    verbose = optVerbose options,
                    overrides = ovs}
      nm = ucFirst name

  (modPrefix, dirPrefix) <- outputPath options

  putStrLn $ "\t* Generating " ++ modPrefix ++ nm
  m <- genModule name cfg allAPIs modPrefix
  let code    = moduleCode m
      modDeps = moduleDeps m
  TIO.writeFile (joinPath [dirPrefix, nm ++ ".hs"]) $
             codeToText code

  putStrLn $ "\t\t+ " ++ modPrefix ++ nm ++ "Attributes"
  m <- genCode cfg allAPIs ["XXX"] (genAttributes name (M.toList apis) modPrefix)
  let attrDeps = moduleDeps m
      attrCode = moduleCode m
  TIO.writeFile (joinPath [dirPrefix, nm ++ "Attributes.hs"]) $
            codeToText attrCode

  putStrLn $ "\t\t+ " ++ modPrefix ++ nm ++ "Signals"
  m <- genCode cfg allAPIs ["XXX"] (genSignalInstances name (M.toList apis) modPrefix)
  let sigCode = moduleCode m
      sigDeps = moduleDeps m
  TIO.writeFile (joinPath [dirPrefix, nm ++ "Signals.hs"]) $
            codeToText sigCode

  when (optCabal options) $ do
    let cabal = "gi-" ++ map toLower nm ++ ".cabal"
    fname <- doesFileExist cabal >>=
             bool (return cabal)
                  (putStrLn (cabal ++ " exists, writing "
                             ++ cabal ++ ".new instead") >>
                   return (cabal ++ ".new"))
    putStrLn $ "\t\t+ " ++ fname
    let usedDeps = S.delete name -- The module is not a dep of itself
                   $ S.unions [modDeps, attrDeps, sigDeps]
        -- We only list as dependencies in the cabal file the
        -- dependencies that we use, disregarding what the .gir file says.
        actualDeps = filter ((`S.member` usedDeps) . T.unpack . girNSName) girDeps
    (err, cabalCode) <- evalCodeGen cfg allAPIs ["XXX"] (genCabalProject gir actualDeps modPrefix)
    case err of
      Nothing -> do
               TIO.writeFile fname (codeToText (moduleCode cabalCode))
               putStrLn "\t\t+ cabal.config"
               TIO.writeFile "cabal.config" cabalConfig
               putStrLn "\t\t+ Setup.hs"
               TIO.writeFile "Setup.hs" setupHs
               putStrLn "\t\t+ LICENSE"
               TIO.writeFile "LICENSE" licenseText
      Just msg -> putStrLn $ "ERROR: could not generate " ++ fname
                  ++ "\nError was: " ++ msg

dump :: Options -> Overrides -> Text -> IO ()
dump options ovs name = do
  let version = M.lookup name (nsChooseVersion ovs)
  (doc, _) <- loadGIRInfo (optVerbose options) name version (optSearchPaths options)
  mapM_ (putStrLn . ppShow) (girAPIs doc)

process :: Options -> [String] -> IO ()
process options names = do
  let extraPaths = optSearchPaths options
  configs <- traverse TIO.readFile (optOverridesFiles options)
  case parseOverridesFile (concatMap T.lines configs) of
    Left errorMsg -> do
      hPutStr stderr "Error when parsing the config file(s):\n"
      hPutStr stderr (T.unpack errorMsg)
      exitFailure
    Right ovs ->
      case optMode options of
        GenerateCode -> forM_ names (processMod options ovs extraPaths)
        Attributes -> genGenericAttrs options ovs (map T.pack names) extraPaths
        Signals -> genGenericConnectors options ovs (map T.pack names) extraPaths
        Dump -> forM_ names (dump options ovs . pack)
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
      names -> process options names
    where
      failWithUsage = do
        hPutStrLn stderr "usage: haskell-gi [options] module1 [module2 [...]]"
        hPutStr stderr showHelp
        exitFailure
