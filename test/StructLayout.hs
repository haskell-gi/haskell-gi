{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, LambdaCase #-}
-- | Check the layout of structs and unions computed by haskell-gi
-- against the C compiler.
--
-- The typelib compiler gets the layout of structs containing
-- bitfields, or inline unions/structs, wrong, so haskell-gi
-- recomputes the layout of these structs (and of the structs
-- embedding them) from the information in the GIR files, see
-- "Data.GI.CodeGen.StructLayout" and
-- <https://github.com/haskell-gi/haskell-gi/issues/485>.
--
-- For every struct and union with fields in the namespaces below
-- (whether its layout was recomputed by haskell-gi or taken from the
-- typelib) we generate a C program checking the size of the struct,
-- the offsets of its fields, and the position and width of its
-- bitfields, and compile it against the actual C headers. Namespaces
-- which are not marked as required are skipped if they are not
-- available.
--
-- Each namespace is checked in a separate process (this program
-- calls itself with the namespace and version as arguments), since
-- different versions of the same namespace (such as Gtk 3 and Gtk 4)
-- cannot be loaded at the same time by libgirepository.
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM, forM_, when)
import Data.List (nub)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getTemporaryDirectory, removeFile, doesFileExist)
import System.Environment (getArgs, getEnvironment, getExecutablePath, lookupEnv)
import System.Exit (ExitCode(..), exitFailure, exitSuccess, exitWith)
import System.IO (hClose, hPutStrLn, openTempFile, stderr)
import System.Process (proc, readCreateProcessWithExitCode,
                       readProcessWithExitCode, CreateProcess(env))
import qualified Text.XML as XML

import Data.GI.CodeGen.API (API(..), GIRInfo(..), loadGIRInfo)
import Data.GI.GIR.BasicTypes (Name(..))
import Data.GI.GIR.Field (Field(..))
import Data.GI.GIR.Repository (readGiRepository)
import Data.GI.GIR.Struct (Struct(..))
import Data.GI.GIR.Union (Union(..))

-- | Namespaces to check: (name, version, required).
namespaces :: [(Text, Text, Bool)]
namespaces = [ ("GLib", "2.0", True)
             , ("GObject", "2.0", True)
             , ("Pango", "1.0", True)
             , ("Gdk", "3.0", False)
             , ("Gtk", "3.0", False)
             , ("Gdk", "4.0", False)
             , ("Gsk", "4.0", False)
             , ("Gtk", "4.0", False)
             , ("Gst", "1.0", False)
             , ("GstBase", "1.0", False)
             , ("GstAudio", "1.0", False)
             , ("GstVideo", "1.0", False)
             , ("GstController", "1.0", False)
             , ("GstRtsp", "1.0", False)
             , ("Poppler", "0.18", False)
             , ("Vte", "2.91", False)
             ]

-- | Structs and unions (by C type) whose layout in the typelib is
-- wrong, but which haskell-gi cannot fix, with the reason.
knownProblems :: M.Map Text Text
knownProblems = M.fromList
  [ ("GThread", "the type of the private \"priority\" field (the deprecated "
                <> "GThreadPriority) is not in the GIR file, so the typelib "
                <> "lays it out as a pointer")
  ]

data Outcome = Passed | Failed | Skipped
  deriving (Eq)

-- | Why a namespace could not be checked successfully.
data Problem = Unavailable String -- ^ The library is not installed.
             | Broken String      -- ^ Some check failed, or we could
                                  -- not run the checks.

-- | Exit code of the child process when the namespace is not
-- available.
unavailableExitCode :: Int
unavailableExitCode = 77

main :: IO ()
main = getArgs >>= \case
  [ns, version] -> checkOne (T.pack ns) (T.pack version)
  [] -> checkAll
  _ -> do
    hPutStrLn stderr "Usage: struct-layout [namespace version]"
    exitFailure

-- | Check a single namespace, signalling the result with the exit
-- code.
checkOne :: Text -> Text -> IO ()
checkOne ns version = checkNamespace ns version >>= \case
  Right () -> exitSuccess
  Left (Unavailable err) -> do
    putStrLn err
    exitWith (ExitFailure unavailableExitCode)
  Left (Broken err) -> do
    putStrLn err
    exitFailure

-- | Check all namespaces, each in its own process.
checkAll :: IO ()
checkAll = do
  self <- getExecutablePath
  outcomes <- forM namespaces $ \(ns, version, required) -> do
    let label = T.unpack ns <> "-" <> T.unpack version
    (code, out, err) <- readProcessWithExitCode self [T.unpack ns, T.unpack version] ""
    case code of
      ExitSuccess -> do
        putStr out
        return Passed
      ExitFailure c | c == unavailableExitCode && not required -> do
        putStrLn ("SKIP " <> label <> ": " <> trim out)
        return Skipped
      ExitFailure _ -> do
        putStr out
        putStr err
        putStrLn ("FAIL " <> label)
        return Failed
  let failed = length (filter (== Failed) outcomes)
      passed = length (filter (== Passed) outcomes)
  putStrLn (show passed <> " namespaces passed, " <> show failed <> " failed, "
            <> show (length outcomes - passed - failed) <> " skipped")
  when (failed > 0) exitFailure
  where trim = T.unpack . T.strip . T.pack

-- | Check all the structs and unions in the given namespace.
checkNamespace :: Text -> Text -> IO (Either Problem ())
checkNamespace ns version = do
  loaded <- try $ do
    (doc, _) <- loadGIRInfo False ns (Just version) [] []
    _ <- evaluate (length (girAPIs doc))
    return doc
  case loaded of
    Left (e :: SomeException) ->
      return (Left (Unavailable ("could not load GIR: " <> show e)))
    Right doc -> do
      includes <- cIncludes ns version
      let checks = concatMap aggregateChecks (girAPIs doc)
      runChecks ns version includes (girPCPackages doc) checks

-- | The C headers to include, as listed in the GIR file.
cIncludes :: Text -> Text -> IO [Text]
cIncludes ns version = do
  XML.Document _ root _ <- readGiRepository False ns (Just version) []
  return [ header | XML.NodeElement e <- XML.elementNodes root
                  , XML.elementName e == cName "include"
                  , Just header <- [M.lookup "name" (XML.elementAttributes e)] ]
  where cName n = XML.Name n (Just "http://www.gtk.org/introspection/c/1.0") (Just "c")

-- | A check, which will be compiled into a C statement.
data Check = Check { checkCType :: Text
                     -- ^ The struct or union being checked.
                   , checkDescription :: Text
                   , checkCode :: Text }

-- | A check, or the reason why we could not check something.
type CheckOrSkip = Either Text Check

aggregateFields :: API -> [Field]
aggregateFields (APIStruct s) = structFields s
aggregateFields (APIUnion u) = unionFields u
aggregateFields _ = []

-- | The checks for the given struct or union.
aggregateChecks :: (Name, API) -> [CheckOrSkip]
aggregateChecks (n, api)
  -- Opaque types.
  | null (aggregateFields api) = []
  | otherwise = case api of
      APIStruct s | not (structIsDisguised s) ->
        withCType (structCType s) $ \ctype ->
          sizeCheck ctype (structSize s) : concatMap (fieldChecks ctype) (structFields s)
      APIUnion u ->
        withCType (unionCType u) $ \ctype ->
          [sizeCheck ctype (unionSize u)]
      _ -> []
  where
    label = namespace n <> "." <> name n

    withCType :: Maybe Text -> (Text -> [Check]) -> [CheckOrSkip]
    withCType Nothing _ = [Left (label <> ": no C type")]
    withCType (Just ctype) checks
      | Just reason <- M.lookup ctype knownProblems =
          [Left (label <> ": known problem, " <> reason)]
      | any unresolvedBitfield (aggregateFields api) =
          [Left (label <> ": the layout of the bitfields could not be determined, "
                 <> "so haskell-gi keeps the typelib layout")]
      | otherwise = map Right (checks ctype)

    unresolvedBitfield f = isJust (fieldBits f) && not (isJust (fieldBitShift f))

    sizeCheck ctype size =
      Check ctype ("sizeof (" <> ctype <> ") == " <> tshow size)
            ("CHECK (sizeof (" <> ctype <> ") == " <> tshow size)

    fieldChecks :: Text -> Field -> [Check]
    fieldChecks ctype f
      -- Anonymous unions and structs cannot be addressed.
      | "<" `T.isPrefixOf` fieldName f = []
      | Just width <- fieldBits f, Just shift <- fieldBitShift f =
          let bit = fieldOffset f * 8 + shift
          in [Check ctype (ctype <> "." <> fieldName f <> " is at bit " <> tshow bit
                     <> " with width " <> tshow width)
                    ("CHECK_BITS (" <> ctype <> ", " <> fieldName f <> ", "
                     <> tshow bit <> ", " <> tshow width)]
      | otherwise =
          [Check ctype (ctype <> "." <> fieldName f <> " is at offset " <> tshow (fieldOffset f))
                 ("CHECK (offsetof (" <> ctype <> ", " <> fieldName f <> ") == "
                  <> tshow (fieldOffset f))]

-- | Compile and run the checks.
runChecks :: Text -> Text -> [Text] -> [Text] -> [CheckOrSkip] -> IO (Either Problem ())
runChecks ns version includes packages checks = do
  let label = T.unpack ns <> "-" <> T.unpack version
      skips = [reason | Left reason <- checks]
  forM_ skips $ \reason -> TIO.putStrLn ("  note: " <> reason)
  pkgConfig <- readProcessWithExitCode "pkg-config"
                 ("--cflags" : "--libs" : map T.unpack packages) ""
  case pkgConfig of
    (ExitFailure _, _, err) -> return (Left (Unavailable ("pkg-config failed: " <> err)))
    (ExitSuccess, flags, _) -> do
      compiled <- compileChecks includes (words flags) [c | Right c <- checks]
      case compiled of
        Left err -> return (Left (Broken err))
        Right (exe, cFile, real) -> do
          (code, out, err) <- readProcessWithExitCode exe [] ""
          putStr out
          putStr err
          removeIfExists exe
          case code of
            ExitSuccess -> do
              putStrLn ("PASS " <> label <> ": " <> show (length real) <> " checks")
              removeIfExists cFile
              return (Right ())
            ExitFailure _ ->
              return (Left (Broken ("some checks failed (" <> cFile <> ")")))

-- | Compile the checks, returning the path to the executable, the C
-- source, and the checks that were compiled. Some records in the GIR
-- files are not declared in the public C headers, or are declared
-- but not defined, so if the compiler complains about some struct
-- or union we drop the corresponding checks and try again.
compileChecks :: [Text] -> [String] -> [Check]
              -> IO (Either String (FilePath, FilePath, [Check]))
compileChecks includes flags = go (10 :: Int)
  where
    go attempts checks = do
      tmpDir <- getTemporaryDirectory
      (cFile, h) <- openTempFile tmpDir "haskell-gi-struct-layout.c"
      TIO.hPutStr h (cProgram includes checks)
      hClose h
      let exe = cFile <> ".exe"
      cc <- fromMaybe "cc" <$> lookupEnv "CC"
      environment <- getEnvironment
      -- Make sure that the error messages are in English, with ASCII quotes.
      let cEnv = ("LC_ALL", "C") : filter ((/= "LC_ALL") . fst) environment
      (code, _, err) <- readCreateProcessWithExitCode
                          ((proc cc (["-w", "-o", exe, cFile] ++ flags))
                            {env = Just cEnv}) ""
      case code of
        ExitSuccess -> return (Right (exe, cFile, checks))
        ExitFailure _ -> do
          let ctypes = S.fromList (map checkCType checks)
              -- Ignore the suggestions ("did you mean 'X'?") in the
              -- error messages, X is defined.
              errors = [ fst (T.breakOn "did you mean" (T.pack l))
                       | l <- lines err, "error" `T.isInfixOf` T.pack l ]
              missing = nub [t | l <- errors, t <- quoted l, t `S.member` ctypes]
          if null missing || attempts <= 1
            then return (Left ("could not compile the checks (" <> cFile <> "):\n" <> err))
            else do
              removeIfExists cFile
              forM_ missing $ \t ->
                TIO.putStrLn ("  note: " <> t <> " is not defined in the C headers")
              go (attempts - 1) (filter ((`notElem` missing) . checkCType) checks)

    -- The strings in single quotes in a compiler error message.
    quoted :: Text -> [Text]
    quoted l = case T.splitOn "'" l of
                 (_ : rest) -> everyOther rest
                 [] -> []
    everyOther (x : _ : rest) = x : everyOther rest
    everyOther [x] = [x]
    everyOther [] = []

removeIfExists :: FilePath -> IO ()
removeIfExists f = do
  exists <- doesFileExist f
  when exists (removeFile f)

-- | The C program running the given checks.
cProgram :: [Text] -> [Check] -> Text
cProgram includes checks = T.unlines $
  [ "#define GST_USE_UNSTABLE_API"
  , "#define GDK_DISABLE_DEPRECATION_WARNINGS"
  , "#define GTK_DISABLE_DEPRECATION_WARNINGS" ]
  ++ map (\h -> "#include <" <> h <> ">") includes ++
  [ "#include <stddef.h>"
  , "#include <stdio.h>"
  , "#include <string.h>"
  , ""
  , "static int checks = 0, failures = 0;"
  , ""
  , "#define CHECK(cond, description) do {                         \\"
  , "    checks++;                                                   \\"
  , "    if (!(cond)) {                                              \\"
  , "      failures++;                                               \\"
  , "      printf (\"  FAIL %s\\n\", description);                    \\"
  , "    }                                                           \\"
  , "  } while (0)"
  , ""
  , "/* Set all the bits of the bitfield in a zeroed struct, and check"
  , "   the number of bits set, and (on little endian machines, where"
  , "   bit i of the struct is bit i % 8 of byte i / 8) the position of"
  , "   the lowest bit set. */"
  , "#define CHECK_BITS(type, field, bit, width, description) do {   \\"
  , "    type s;                                                     \\"
  , "    memset (&s, 0, sizeof s);                                   \\"
  , "    s.field = ~0;                                               \\"
  , "    CHECK ((G_BYTE_ORDER != G_LITTLE_ENDIAN                     \\"
  , "            || lowest_bit (&s, sizeof s) == (bit))              \\"
  , "           && bits_set (&s, sizeof s) == (width), description); \\"
  , "  } while (0)"
  , ""
  , "static int lowest_bit (const void *p, size_t size) {"
  , "  const unsigned char *bytes = p;"
  , "  for (size_t i = 0; i < size * 8; i++)"
  , "    if (bytes[i / 8] & (1u << (i % 8)))"
  , "      return (int) i;"
  , "  return -1;"
  , "}"
  , ""
  , "static int bits_set (const void *p, size_t size) {"
  , "  const unsigned char *bytes = p;"
  , "  int count = 0;"
  , "  for (size_t i = 0; i < size * 8; i++)"
  , "    if (bytes[i / 8] & (1u << (i % 8)))"
  , "      count++;"
  , "  return count;"
  , "}"
  , ""
  , "int main (void) {" ]
  ++ map (\c -> "  " <> checkCode c <> ", " <> cString (checkDescription c) <> ");")
         checks ++
  [ "  if (failures > 0)"
  , "    printf (\"  %d out of %d checks failed\\n\", failures, checks);"
  , "  return failures != 0;"
  , "}" ]

cString :: Text -> Text
cString t = "\"" <> T.concatMap escape t <> "\""
  where escape '"' = "\\\""
        escape '\\' = "\\\\"
        escape c = T.singleton c

tshow :: Show a => a -> Text
tshow = T.pack . show
