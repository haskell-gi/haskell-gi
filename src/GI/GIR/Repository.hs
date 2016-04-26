module GI.GIR.Repository (readGiRepository) where

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
import System.FilePath

girDataDirs :: IO [FilePath]
girDataDirs = getSystemDataDirs "gir-1.0"

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

-- | Search for an appropriate @.gir@ file in the search path. This is
-- either passed in explicitly, or if that is absent, loaded from the
-- environment variable @HASKELL_GI_GIR_SEARCH_PATH@. In either case
-- the system data dirs are also searched if nothing can be found in
-- the explicitly passed paths, or in the contents of
-- @HASKELL_GI_GIR_SEARCH_PATH@.
girFile :: Text -> Maybe Text -> [FilePath] -> IO (Maybe FilePath)
girFile name version [] =
    lookupEnv "HASKELL_GI_GIR_SEARCH_PATH" >>= \case
      Nothing -> girFile name version []
      Just s -> girFile name version (splitOn ':' s)
girFile name version extraPaths = do
  dataDirs <- girDataDirs
  firstJust <$> (mapM (girFile' name version) (extraPaths ++ dataDirs))
    where firstJust = listToMaybe . catMaybes

-- | Try to load the `.gir` file corresponding to the given repository
readGiRepository :: Bool        -- ^ verbose
                 -> Text        -- ^ name
                 -> Maybe Text  -- ^ version
                 -> [FilePath]  -- ^ searchPath
                 -> IO XML.Document
readGiRepository verbose name version extraPaths =
    girFile name version extraPaths >>= \case
        Just path -> do
            when verbose $ putStrLn $ "Loading GI repository: " ++ path
            XML.readFile XML.def path
        Nothing -> do
            dataDirs <- girDataDirs
            error $ "Did not find a GI repository for " ++ (T.unpack name)
                ++ maybe "" ("-" ++) (T.unpack <$> version)
                ++ " in " ++ show dataDirs
