module Main where

import Control.Applicative ((<$>))
import Control.Exception (handle)
import Data.Maybe (mapMaybe)
import System.Console.GetOpt
import System.Exit
import System.IO (hPutStr, hPutStrLn, stderr)

import qualified Data.Map as M

--import Graphics.UI.Gtk
import System.Glib.Initialize
import System.Glib.GError
import Text.Show.Pretty

import GI.API (loadAPI)
import GI.Code (Config(..), codeToString, runCodeGen')
import GI.CodeGen (genModule)
import GI.Shlib (typelibSymbols, strip)
import GI.GObject (parseObjectHierarchy)

data Mode = GenerateCode | Dump | Help
  deriving Show

data Options = Options {
  optImports :: [String],
  optMode :: Mode,
  optRenames :: [(String, String)],
  optPrefixes :: [(String, String)] }
    deriving Show

defaultOptions = Options {
  optImports = [],
  optMode = GenerateCode,
  optRenames = [],
  optPrefixes = [] }

parseKeyValue s =
  let (a, ('=':b)) = break (=='=') s
   in (a, b)

optDescrs :: [OptDescr (Options -> Options)]
optDescrs = [
  Option "h" ["help"] (NoArg $ \opt -> opt { optMode = Help })
    "print this gentle help text",
  Option "i" ["import"] (ReqArg
    (\arg opt -> opt { optImports = arg : optImports opt }) "IMPORT")
    "add an import to the generated code",
  Option "d" ["dump"] (NoArg $ \opt -> opt { optMode = Dump })
    "dump internal representation instead of generating code",
  Option "p" ["prefix"] (ReqArg
    (\arg opt ->
      let (a, b) = parseKeyValue arg
       in opt { optPrefixes = (a, b) : optPrefixes opt }) "A=B")
     "specify the prefix for a particular namespace",
  Option "r" ["rename"] (ReqArg
    (\arg opt ->
      let (a, b) = parseKeyValue arg
       in opt { optRenames = (a, b) : optRenames opt }) "A=B")
    "specify a Haskell name for a C name"]

showHelp = concat $ map optAsLine optDescrs
  where optAsLine (Option flag (long:_) _ desc) =
          "  -" ++ flag ++ "|--" ++ long ++ "\t" ++ desc ++ "\n"
        optAsLine _ = error "showHelp"

printGError = handle (\(GError _dom _code msg) -> putStrLn msg)

processAPI options name = do
    syms <- typelibSymbols name
    apis <- mapMaybe (\(n, a) ->
                  case strip syms a of
                    Nothing -> Nothing
                    Just a' -> Just (n, a')) <$>
            loadAPI name
    imported <- mapM (\n -> M.fromList `fmap` loadAPI n) (optImports options)
    let input' = foldl M.union (M.fromList apis) imported
    let cfg = Config {
          imports = optImports options,
          prefixes = M.fromList (optPrefixes options),
          names = M.fromList (optRenames options),
          input = input',
          instances = parseObjectHierarchy input',
          modName = name }

    case optMode options of
        GenerateCode ->
            putStrLn $ codeToString $ runCodeGen' cfg $ genModule name apis
        Dump -> mapM_ (putStrLn . ppShow) apis
        Help -> putStr showHelp

main = printGError $ do
    args <- initArgs
    let (actions, nonOptions, errors) = getOpt RequireOrder optDescrs args
        options  = foldl (.) id actions defaultOptions

    case errors of
        [] -> return ()
        _ -> do
            mapM_ (hPutStr stderr) errors
            exitFailure

    case nonOptions of
        [name] -> processAPI options name
        _ -> do
            hPutStrLn stderr "usage: haskell-gi [options] package"
            hPutStr stderr showHelp
            exitFailure
