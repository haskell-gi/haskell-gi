module Main where

import Control.Applicative ((<$>))
import Control.Exception (handle)
import Control.Monad (forM_)
import Data.Maybe (mapMaybe)
import Data.List (intercalate)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (splitPath, joinPath)
import System.Console.GetOpt
import System.Exit
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Environment (getArgs)

import qualified Data.Map as M
import qualified Data.Set as Set

import GI.Utils.GError
import Text.Show.Pretty (ppShow)

import GI.API (loadAPI, API(..), Name(..))
import GI.Code (Config(..), codeToString, genCode)
import GI.CodeGen (genModule)
import GI.Lenses (genLenses, genAllLenses)
import GI.SymbolNaming (ucFirst)
import GI.Shlib (typelibSymbols, strip)
import GI.Internal.Typelib (prependSearchPath)

data Mode = GenerateCode | Dump | Help
  deriving Show

data Options = Options {
  optImports :: [String],
  optMode :: Mode,
  optOutput :: Maybe String,
  optRenames :: [(String, String)],
  optSearchPaths :: [String],
  optPrefixes :: [(String, String)] }
    deriving Show

defaultOptions = Options {
  optImports = [],
  optMode = GenerateCode,
  optOutput = Just "GI",
  optRenames = [],
  optSearchPaths = [],
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
  Option "o" ["output"] (ReqArg
                         (\arg opt -> opt {optOutput = Just arg}) "OUTPUT")
    "set the output directory",
  Option "p" ["prefix"] (ReqArg
    (\arg opt ->
      let (a, b) = parseKeyValue arg
       in opt { optPrefixes = (a, b) : optPrefixes opt }) "A=B")
     "specify the prefix for a particular namespace",
  Option "s" ["search"] (ReqArg
    (\arg opt -> opt { optSearchPaths = arg : optSearchPaths opt }) "PATH")
    "prepend a directory to the typelib search path",
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

loadAPI' :: String -> IO [(Name, API)]
loadAPI' name = do
    syms <- typelibSymbols name
    mapMaybe (\(n, a) ->
              case strip syms a of
                Nothing -> Nothing
                Just a' -> Just (n, a')) <$>
           loadAPI name

outputPath :: Options -> IO (String, String) -- modPrefix, dirPrefix
outputPath options =
    case optOutput options of
      Nothing -> return ("", ".")
      Just dir -> do
        createDirectoryIfMissing True dir
        let prefix = intercalate "." (splitPath dir) ++ "."
        return (prefix, dir)

moduleConfig :: String -> Options -> Config
moduleConfig name options = Config {
              prefixes = M.fromList (optPrefixes options),
              names = M.fromList (optRenames options),
              modName = name,
              ignoredMethods = ignore }

-- Generate all generic accessor functions ("_label", for example).
genGenericAttrs :: Options -> [String] -> IO ()
genGenericAttrs options modules = do
  allAPIs <- (M.toList . M.unions . (map M.fromList)) <$> mapM loadAPI modules
  (modPrefix, dirPrefix) <- outputPath options
  putStrLn $ "\t* Generating " ++ modPrefix ++ "Properties"
  (_, code) <- genCode (moduleConfig "Properties" options) $
                                  genAllLenses allAPIs modPrefix
  writeFile (joinPath [dirPrefix,  "Properties.hs"]) $ codeToString code

-- Generate the code for the given module, and return the dependencies
-- for this module.
processMod :: Options -> String -> IO (Set.Set String)
processMod options name = do
  apis <- loadAPI' name

  let cfg = moduleConfig name options
      nm = ucFirst name

  (modPrefix, dirPrefix) <- outputPath options

  putStrLn $ "\t* Generating " ++ modPrefix ++ nm

  (deps, code) <- genCode cfg $ genModule name apis modPrefix
  writeFile (joinPath [dirPrefix, nm ++ ".hs"]) $
             codeToString code
  (lensDeps, lensCode) <- genCode cfg $ genLenses name apis modPrefix
  writeFile (joinPath [dirPrefix, nm ++ "Attributes.hs"]) $
            codeToString lensCode
  return $ name `Set.delete` (Set.union deps lensDeps)

generateModules :: Options -> Set.Set String -> Set.Set String -> IO ()
generateModules options targets done
    | targets == Set.empty = genGenericAttrs options (Set.toList done)
    | otherwise = do
  let mod = head (Set.toList targets)
  deps <- processMod options mod
  let done' = Set.insert mod done
      targets' = Set.difference (Set.union targets deps) done'
  generateModules options targets' done'

dump :: [String] -> IO ()
dump modules =
    forM_ modules $ \name -> do
      apis <- loadAPI' name
      mapM_ (putStrLn . ppShow) apis >> return Set.empty

process :: Options -> [String] -> IO ()
process options modules = do
  mapM_ prependSearchPath $ optSearchPaths options
  case optMode options of
      GenerateCode -> generateModules options (Set.fromList modules) Set.empty
      Dump -> dump modules
      Help -> putStr showHelp

foreign import ccall "g_type.h g_type_init"
    g_type_init :: IO ()

main :: IO ()
main = printGError $ do
    g_type_init -- Initialize GLib's type system
    args <- getArgs
    let (actions, nonOptions, errors) = getOpt RequireOrder optDescrs args
        options  = foldl (.) id actions defaultOptions

    case errors of
        [] -> return ()
        _ -> do
            mapM_ (hPutStr stderr) errors
            exitFailure

    case nonOptions of
        [] -> do
            hPutStrLn stderr "usage: haskell-gi [options] package1 [package2 ...]"
            hPutStr stderr showHelp
            exitFailure
        names -> process options names

-- XXX We should read this from an external file, specified
-- from the command line.
ignore = ["atk_editable_text_set_run_attributes"
         , "atk_text_get_run_attributes"
         , "atk_text_get_default_attributes"
         , "atk_object_get_attributes"
         , "atk_document_get_attributes"
         -- This accepts a NULL-terminated array of
         -- strings, but it is not marked as such in
         -- the bindings.
         , "g_file_info_set_attribute_stringv"
         -- It returns an array of arrays of strings, we do not
         -- support this yet.
         , "g_desktop_app_info_search"
         -- The size of the array depends on the
         -- second argument in a nontrivial way.
         , "g_inet_address_new_from_bytes"
         -- This is listed as a method of
         -- GObject.Object, but the object
         -- parameter to the function is missing.
         , "g_object_interface_find_property"
         -- The same for the other
         -- g_object_interface_* functions
         , "g_object_interface_install_property"
         , "g_object_interface_list_properties"
         -- caller-allocates, which we don't support
         -- yet.
         , "g_io_channel_read_chars"
         -- The length of the array is marked as the
         -- 0th argument, but it is the 2nd.
         , "g_io_channel_write_chars"
         -- "count" argument is not marked as out.
         , "g_bookmark_file_get_app_info"
         -- "length" is not marked as out
         , "g_bookmark_file_to_data"
         -- "terminator_pos" is not marked as out.
         , "g_io_channel_read_line_string"
         -- length and terminator_pos are not marked as out.
         , "g_io_channel_read_line"
         -- "line_number" is not marked as out.
         , "g_markup_parse_context_get_position"
         -- The closure annotations on the closure
         -- themselves are wrong, they point to the
         -- callbacks.
         , "g_desktop_app_info_launch_uris_as_manager"
         -- The length of the array is given by a
         -- call to pango_tab_array_get_size(), not
         -- an argument.
         , "pango_tab_array_get_tabs"
         -- Length of log_attrs is not given
         , "pango_glyph_item_letter_space"
         -- The length of logical_widths is not
         -- given by an argument.
         , "pango_glyph_string_get_logical_widths"
         -- The length of logical_widths is not
         -- given by an argument.
         , "pango_glyph_item_get_logical_widths"
         -- The length argument is an out value,
         -- but it is nto marked as such in the
         -- bindings.
         , "g_tls_password_get_value"
         -- This clashes with the corresponding
         -- property getter.
         , "gtk_widget_is_focus"
         -- Same for some other properties
         , "gtk_widget_has_focus"
         , "gtk_widget_has_default"
         , "gtk_mount_operation_is_showing"
         , "gtk_window_has_toplevel_focus"
         , "gtk_window_is_active"
         -- The size of the array depends on a
         -- complicated combination of the rest of
         -- the arguments.
         , "gdk_pixbuf_new_from_data"]
