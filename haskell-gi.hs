module Main where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Exception (handle)
import Data.List (intercalate)
import Data.Text (unpack)
import Data.Traversable (traverse)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (splitPath, joinPath)
import System.Console.GetOpt
import System.Exit
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Environment (getArgs)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import GI.Utils.GError
import Text.Show.Pretty (ppShow)

import GI.API (loadAPI)
import GI.Code (codeToString, genCode)
import GI.Config (Config(..), parseConfigFile)
import GI.CodeGen (genModule)
import GI.Attributes (genAttributes, genAllAttributes)
import GI.SymbolNaming (ucFirst)
import GI.Internal.Typelib (prependSearchPath)

data Mode = GenerateCode | Dump | Attributes | Help
  deriving Show

data Options = Options {
  optMode :: Mode,
  optOutput :: Maybe String,
  optConfigFiles :: [String],
  optRenames :: [(String, String)],
  optSearchPaths :: [String],
  optVerbose :: Bool}
    deriving Show

defaultOptions = Options {
  optMode = GenerateCode,
  optOutput = Just "GI",
  optConfigFiles = [],
  optRenames = [],
  optSearchPaths = [],
  optVerbose = False }

parseKeyValue s =
  let (a, ('=':b)) = break (=='=') s
   in (a, b)

optDescrs :: [OptDescr (Options -> Options)]
optDescrs = [
  Option "h" ["help"] (NoArg $ \opt -> opt { optMode = Help })
    "print this gentle help text",
  Option "a" ["attributes"] (NoArg $ \opt -> opt {optMode = Attributes})
    "generate generic attribute accesors",
  Option "c" ["config"] (ReqArg
                           (\arg opt -> opt {optConfigFiles =
                                                 arg : optConfigFiles opt})
                          "CONFIG")
    "specify a config file for the code generator",
  Option "d" ["dump"] (NoArg $ \opt -> opt { optMode = Dump })
    "dump internal representation instead of generating code",
  Option "o" ["output"] (ReqArg
                         (\arg opt -> opt {optOutput = Just arg}) "OUTPUT")
    "set the output directory",
  Option "r" ["rename"] (ReqArg
    (\arg opt ->
      let (a, b) = parseKeyValue arg
       in opt { optRenames = (a, b) : optRenames opt }) "A=B")
    "specify a Haskell name for a C name",
  Option "s" ["search"] (ReqArg
    (\arg opt -> opt { optSearchPaths = arg : optSearchPaths opt }) "PATH")
    "prepend a directory to the typelib search path",
  Option "v" ["verbose"] (NoArg $ \opt -> opt { optVerbose = True })
    "print extra info while processing"]

showHelp = concat $ map optAsLine optDescrs
  where optAsLine (Option flag (long:_) _ desc) =
          "  -" ++ flag ++ "|--" ++ long ++ "\t" ++ desc ++ "\n"
        optAsLine _ = error "showHelp"

printGError = handle (\(GError _dom _code msg) -> putStrLn (unpack msg))

outputPath :: Options -> IO (String, String) -- modPrefix, dirPrefix
outputPath options =
    case optOutput options of
      Nothing -> return ("", ".")
      Just dir -> do
        createDirectoryIfMissing True dir
        let prefix = intercalate "." (splitPath dir) ++ "."
        return (prefix, dir)

-- Generate all generic accessor functions ("_label", for example).
genGenericAttrs :: Options -> Config -> [String] -> IO ()
genGenericAttrs options cfg modules = do
  allAPIs <- (M.toList . M.unions . (map M.fromList))
             <$> mapM (loadAPI (optVerbose options)) modules
  (modPrefix, dirPrefix) <- outputPath options
  putStrLn $ "\t* Generating " ++ modPrefix ++ "Properties"
  (_, code) <- genCode cfg (genAllAttributes allAPIs modPrefix)
  writeFile (joinPath [dirPrefix, "Properties.hs"]) $ codeToString code

-- Generate the code for the given module, and return the dependencies
-- for this module.
processMod :: Options -> Config -> String -> IO ()
processMod options generalConfig name = do
  apis <- loadAPI (optVerbose options) name

  let cfg = generalConfig {modName = Just name}
      nm = ucFirst name

  (modPrefix, dirPrefix) <- outputPath options

  putStrLn $ "\t* Generating " ++ modPrefix ++ nm
  (_, code) <- genCode cfg (genModule name apis modPrefix)
  writeFile (joinPath [dirPrefix, nm ++ ".hs"]) $
             codeToString code

  putStrLn $ "\t\t+ " ++ modPrefix ++ nm ++ "Attributes"
  (_, attrCode) <- genCode cfg (genAttributes name apis modPrefix)
  writeFile (joinPath [dirPrefix, nm ++ "Attributes.hs"]) $
            codeToString attrCode

  putStrLn $ "\t\t+ " ++ modPrefix ++ nm ++ "Signals"
  -- XXX overloaded signal handlers, to be done.

dump :: Options -> String -> IO ()
dump options name = do
  apis <- loadAPI (optVerbose options) name
  mapM_ (putStrLn . ppShow) apis

process :: Options -> [String] -> IO ()
process options names = do
  mapM_ prependSearchPath $ optSearchPaths options
  configs <- traverse TIO.readFile (optConfigFiles options)
  case parseConfigFile (concatMap T.lines configs) (optVerbose options) of
    Left errorMsg -> do
      hPutStr stderr "Error when parsing the config file(s):\n"
      hPutStr stderr (T.unpack errorMsg)
      exitFailure
    Right cfg -> case optMode options of
                   GenerateCode -> forM_ names (processMod options cfg)
                   Attributes -> genGenericAttrs options cfg names
                   Dump -> forM_ names (dump options)
                   Help -> putStr showHelp

foreign import ccall "g_type.h g_type_init"
    g_type_init :: IO ()

main :: IO ()
main = printGError $ do
    g_type_init -- Initialize GLib's type system.
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
