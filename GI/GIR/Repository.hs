{-# LANGUAGE OverloadedStrings #-}
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
import Text.XML

import System.Directory
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

girFile :: Text -> Maybe Text -> [FilePath] -> IO (Maybe FilePath)
girFile name version extraPaths = do
  dataDirs <- girDataDirs
  firstJust <$> (mapM (girFile' name version) (extraPaths ++ dataDirs))
    where firstJust = listToMaybe . catMaybes

readGiRepository :: Bool -> Text -> Maybe Text -> [FilePath] -> IO Document
readGiRepository verbose name version extraPaths =
    girFile name version extraPaths >>= \case
        Just path -> do
            when verbose $ putStrLn $ "Loading GI repository: " ++ path
            readFile def path
        Nothing -> do
            dataDirs <- girDataDirs
            error $ "Did not find a GI repository for " ++ (T.unpack name)
                ++ maybe "" ("-" ++) (T.unpack <$> version)
                ++ " in " ++ show dataDirs
