module GI.Repository
    ( readGiRepository
    ) where

import Prelude hiding (readFile)

import Control.Applicative ((<|>))
import Control.Monad (liftM, when)
import qualified Data.List as List
import Data.Bool (bool)
import Data.Maybe
import Safe (maximumMay)
import Text.XML

import System.Directory
import System.Environment.XDG.BaseDir (getSystemDataDirs)
import System.FilePath

girDataDirs :: IO [FilePath]
girDataDirs = getSystemDataDirs "gir-1.0"

girFile' :: String -> Maybe String -> FilePath -> IO (Maybe FilePath)
girFile' name version path = do
    dirExists <- doesDirectoryExist path
    if dirExists then do
        repositories <- liftM takeBaseName <$> getDirectoryContents path
        let maxAvailVersion = maximumMay . catMaybes $
                List.stripPrefix (name ++ "-") <$> repositories
            filePath = (path </>) . (name ++) . ("-" ++) . (<.> "gir")
                <$> (version <|> maxAvailVersion)

        doesFileExist (fromMaybe "" filePath) >>= bool
            (return Nothing)
            (return filePath)
    else
        return Nothing

girFile :: String -> Maybe String -> IO (Maybe FilePath)
girFile name version = listToMaybe <$> catMaybes <$>
    (girDataDirs >>= mapM (girFile' name version))

readGiRepository :: Bool -> String -> Maybe String -> IO Document
readGiRepository verbose name version =
    girFile name version >>= maybe
        (do dataDirs <- girDataDirs
            error $ "Did not find a GI repository for " ++ name
                ++ maybe "" ("-" ++) version
                ++ " in " ++ show dataDirs)
        (\path -> do
            when verbose $ putStrLn $ "Loading GI repository: " ++ path
            readFile def path)
