{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module GI.Repository
    ( readGiRepository
    , girNamespaceCursor
    , girNamespaceName
    ) where

import Prelude hiding (readFile)

import Control.Monad (when)
import qualified Data.List as List
import Data.Maybe
import Data.Text (Text)
import Safe (maximumMay)
import Text.XML
import Text.XML.Cursor

import System.Directory
import System.Environment.XDG.BaseDir (getSystemDataDirs)
import System.FilePath

girDataDirs :: IO [FilePath]
girDataDirs = getSystemDataDirs "gir-1.0"

girFilePath :: String -> String -> FilePath -> FilePath
girFilePath name version path = path </> name ++ "-" ++ version <.> "gir"

girFile' :: String -> Maybe String -> FilePath -> IO (Maybe FilePath)
girFile' name (Just version) path =
    let filePath = girFilePath name version path
    in  doesFileExist filePath >>= \case
        True  -> return $ Just filePath
        False -> return Nothing
girFile' name Nothing path =
    doesDirectoryExist path >>= \case
        True -> do
            repositories <- map takeBaseName <$> getDirectoryContents path
            let version = maximumMay . catMaybes $
                    List.stripPrefix (name ++ "-") <$> repositories

            return $ case version of
                Just v  -> Just $ girFilePath name v path
                Nothing -> Nothing

        False -> return Nothing

girFile :: String -> Maybe String -> IO (Maybe FilePath)
girFile name version =
    firstJust <$> (girDataDirs >>= mapM (girFile' name version))
    where firstJust = listToMaybe . catMaybes

readGiRepository :: Bool -> String -> Maybe String -> IO Document
readGiRepository verbose name version =
    girFile name version >>= \case
        Just path -> do
            when verbose $ putStrLn $ "Loading GI repository: " ++ path
            readFile def path
        Nothing -> do
            dataDirs <- girDataDirs
            error $ "Did not find a GI repository for " ++ name
                ++ maybe "" ("-" ++) version
                ++ " in " ++ show dataDirs

girNamespaceCursor' :: Document -> [Cursor]
girNamespaceCursor' doc =
    fromDocument doc $/ laxElement "namespace"

girNamespaceCursor :: Document -> Cursor
girNamespaceCursor = head . girNamespaceCursor'

girNamespaceName :: Document -> Maybe Text
girNamespaceName doc =
    case girNamespaceCursor' doc >>= attribute "name" of
        [text] -> Just text
        _      -> Nothing
