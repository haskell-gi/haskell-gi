{-# LANGUAGE OverloadedStrings #-}

import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (joinPath)
import System.IO (hPutStr, hPutStrLn, stderr)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text (Text)

import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP

import ProjectInfo (ProjectInfo(..), prettyConfig)

data Options = Options {
  -- | A message to prepend to @ChangeLog.md@ explaining the reason
  -- for the version bump.
  changeLogMessage :: Maybe Text
  }

defaultOptions :: Options
defaultOptions = Options {
  changeLogMessage = Nothing
  }

optDescrs :: [OptDescr (Options -> Options)]
optDescrs = [
  Option "m" ["message"] (ReqArg
                         (\msg opt -> opt {changeLogMessage = Just (T.pack msg)})
                         "MESSAGE")
    "Prepend a message to ChangeLog.md explaining the version bump"
  ]

showHelp :: String
showHelp = concatMap optAsLine optDescrs
  where optAsLine (Option flag (long:_) _ desc) =
          "  -" ++ flag ++ "|--" ++ long ++ "\t" ++ desc ++ "\n"
        optAsLine _ = error "showHelp"

bumpMinor :: Text -> Text
bumpMinor = T.intercalate "." . map (T.pack . show)
            . increaseMinor
            . map (read . T.unpack) . T.splitOn "."
            where increaseMinor :: [Int] -> [Int]
                  increaseMinor [] = []
                  increaseMinor [m] = [m+1]
                  increaseMinor (v:vs) = v : increaseMinor vs

writeChangeLog :: Text -> Text -> FilePath -> IO ()
writeChangeLog msg newMinor fname = do
  oldContents <- B.readFile fname
  let newContents = TE.encodeUtf8 $ T.unlines $
        [ "### " <> newMinor
        , ""
        , "+ " <> msg
        , ""
        ]
  B.writeFile fname (newContents <> oldContents)

modifyPkgInfo :: Text -> FilePath -> IO ()
modifyPkgInfo msg dirName = do
  let fname = joinPath [dirName, "pkg.info"]
  contents <- B.readFile fname
  case A.eitherDecodeStrict contents of
    Left err -> error ("Could not parse " <> show fname <> ": " <> err)
    Right info -> do
      let newMinor = bumpMinor (version info)
      writeChangeLog msg newMinor (joinPath [dirName, "ChangeLog.md"])
      B.writeFile fname (BL.toStrict $ AP.encodePretty' prettyConfig
                          (info {version = newMinor}))

main :: IO ()
main = do
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
      hPutStrLn stderr "Error: no pkg.info provided."
      failWithUsage
    names -> case changeLogMessage options of
      Nothing -> do
        hPutStrLn stderr "Error: no ChangeLog message provided."
        failWithUsage
      Just m -> mapM_ (modifyPkgInfo m) names

  where
    failWithUsage = do
      hPutStrLn stderr "usage: bumpMinor -m \"Message\" dir1 [dir2 [...]]"
      hPutStr stderr showHelp
      exitFailure
