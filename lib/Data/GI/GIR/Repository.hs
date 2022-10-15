module Data.GI.GIR.Repository (readGiRepository) where

import Prelude hiding (readFile)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Control.Monad (when)
import Data.Maybe
import qualified Data.List as List
import qualified Data.Text as T
import Data.Text (Text)
import Safe (maximumMay)
import qualified Text.XML as XML

import System.Directory
import System.Environment (lookupEnv)
import System.Environment.XDG.BaseDir (getSystemDataDirs)
import System.FilePath (searchPathSeparator, takeBaseName, (</>), (<.>))

girFilePath :: String -> String -> FilePath -> FilePath
girFilePath name version path = path </> name ++ "-" ++ version <.> "gir"

girFile' :: Text -> Maybe Text -> FilePath -> IO (Maybe FilePath)
girFile' name (Just version) path =
    let filePath = girFilePath (T.unpack name) (T.unpack version) path
    in  doesFileExist filePath >>= \case
        True  -> return $ Just filePath
        False -> return Nothing
girFile' name Nothing path =
    doesDirectoryExist path >>= \case
        True -> do
            repositories <- map takeBaseName <$> getDirectoryContents path
            let version = maximumMay . catMaybes $
                    List.stripPrefix (T.unpack name ++ "-") <$> repositories

            return $ case version of
                Just v  -> Just $ girFilePath (T.unpack name) v path
                Nothing -> Nothing

        False -> return Nothing

-- | Split a list into sublists delimited by the given element.
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x xs = go xs []
    where go [] acc = [reverse acc]
          go (y : ys) acc = if x == y
                            then reverse acc : go ys []
                            else go ys (y : acc)

-- | Return the paths where to look for gir files.
girDataDirs :: IO [FilePath]
girDataDirs = do
  sys <- getSystemDataDirs "gir-1.0"
  -- See https://github.com/haskell-gi/haskell-gi/issues/390
  let macOS = ["/opt/homebrew/share/gir-1.0"]
  return (sys ++ macOS)

-- | Construct the GIR search path, possibly looking into the
-- @HASKELL_GI_GIR_SEARCH_PATH@ environment variable if no explicit
-- list of extra paths is given. In either case
-- the system data dirs are also searched if nothing can be found in
-- the explicitly passed paths, or in the contents of
-- @HASKELL_GI_GIR_SEARCH_PATH@.
buildSearchPath :: [FilePath] -> IO [FilePath]
buildSearchPath extraPaths = do
  paths <- case extraPaths of
             [] -> lookupEnv "HASKELL_GI_GIR_SEARCH_PATH" >>= \case
               Nothing -> return []
               Just s -> return (splitOn searchPathSeparator s)
             ps -> return ps
  dataDirs <- girDataDirs
  return (paths ++ dataDirs)

-- | Search for an appropriate @.gir@ file in the search path.
girFile :: Text -> Maybe Text -> [FilePath] -> IO (Maybe FilePath)
girFile name version searchPath =
  firstJust <$> (mapM (girFile' name version) searchPath)
    where firstJust = listToMaybe . catMaybes

-- | Try to load the `.gir` file corresponding to the given repository
readGiRepository :: Bool        -- ^ verbose
                 -> Text        -- ^ name
                 -> Maybe Text  -- ^ version
                 -> [FilePath]  -- ^ searchPath
                 -> IO XML.Document
readGiRepository verbose name version extraPaths = do
  searchPath <- buildSearchPath extraPaths
  girFile name version searchPath >>= \case
        Just path -> do
            when verbose $ putStrLn $ "Loading GI repository: " ++ path
            XML.readFile XML.def path
        Nothing -> error $ "Did not find a GI repository for "
                   ++ (T.unpack name)
                   ++ maybe "" ("-" ++) (T.unpack <$> version)
                   ++ " in " ++ show searchPath ++ "."
