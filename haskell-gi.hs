module Main where

import Control.Applicative ((<$>))
import Control.Exception (handle)
import Data.Maybe (mapMaybe)
import Data.List (intercalate)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (joinPath)
import System.Console.GetOpt
import System.Exit
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Environment (getArgs)

import qualified Data.Map as M

import GI.Utils.GError
import Text.Show.Pretty

import GI.API (loadAPI)
import GI.Code (Config(..), codeToString, runCodeGen')
import GI.CodeGen (genModule)
import GI.GObject (parseObjectHierarchy)
import GI.Lenses (genLenses)
import GI.SymbolNaming (ucFirst)
import GI.Shlib (typelibSymbols, strip)
import GI.Util (split)

data Mode = GenerateCode | Dump | Help
  deriving Show

data Options = Options {
  optImports :: [String],
  optMode :: Mode,
  optModuleName :: Maybe String,
  optRenames :: [(String, String)],
  optPrefixes :: [(String, String)] }
    deriving Show

defaultOptions = Options {
  optImports = [],
  optMode = GenerateCode,
  optModuleName = Nothing,
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
  Option "m" ["module"] (ReqArg
    (\arg opt -> opt { optModuleName = Just arg}) "MODNAME")
    "Generated module name (something like GI.GObject)",
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
          modName = name,
          -- XXX We should read this from an external file, specified
          -- from the command line.
          ignoredMethods = ["atk_editable_text_set_run_attributes"
                           , "atk_text_get_run_attributes"
                           , "atk_text_get_default_attributes"
                           , "atk_object_get_attributes"
                           , "atk_document_get_attributes"
                           -- This accepts a NULL-terminated array of
                           -- strings, but it is not marked as such in
                           -- the bindings.
                           , "g_file_info_set_attribute_stringv"
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
                           , "gdk_pixbuf_new_from_data"]}

    case optMode options of
        GenerateCode -> do
          let mn = case optModuleName options of
                          Nothing -> ucFirst name
                          Just s -> s
          (modPrefix, dirPrefix, modName) <- case split '.' mn of
            [] -> error $ "Empty module name"
            [m] -> return ("", ".", m)
            xs -> do
              let installDir = joinPath $ init xs
              createDirectoryIfMissing True installDir
              return (intercalate "." (init xs) ++ ".", installDir, last xs)
          writeFile (joinPath [dirPrefix, modName ++ ".hs"]) $
             codeToString $ runCodeGen' cfg $ genModule modName apis modPrefix
          writeFile (joinPath [dirPrefix, modName ++ "Attributes.hs"]) $
             codeToString $ runCodeGen' cfg $
                        genLenses modName apis (M.toList input') modPrefix
        Dump -> mapM_ (putStrLn . ppShow) apis
        Help -> putStr showHelp

foreign import ccall "g_type.h g_type_init"
    g_type_init :: IO ()

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
        [name] -> processAPI options name
        _ -> do
            hPutStrLn stderr "usage: haskell-gi [options] package"
            hPutStr stderr showHelp
            exitFailure
