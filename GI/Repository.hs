{-# LANGUAGE OverloadedStrings #-}

module GI.Repository
    ( readGiRepository
    , girNamespaceElem
    , girNamespace
    , girVersion
    , girPackage
    , girIncludes
    , nodeToElement
    ) where

import Prelude hiding (readFile)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Control.Monad (when)
import qualified Data.List as List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Safe (maximumMay)
import Text.XML

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

girNamespaceElem :: Document -> Maybe Element
girNamespaceElem = listToMaybe . topLevelChildsWithLocalName "namespace"

girNamespace :: Document -> Maybe String
girNamespace doc = do
    namespaceNode <- girNamespaceElem doc
    namespaceText <- M.lookup "name" $ elementAttributes namespaceNode
    return $ T.unpack namespaceText

girVersion :: Document -> Maybe String
girVersion doc = do
    namespaceNode <- girNamespaceElem doc
    versionText <- M.lookup "version" $ elementAttributes namespaceNode
    return $ T.unpack versionText

girPackage :: Document -> Maybe String
girPackage doc = do
    packageNode <- listToMaybe $ topLevelChildsWithLocalName "package" doc
    packageText <- M.lookup "name" $ elementAttributes packageNode
    return $ T.unpack packageText

-- This is just awful ._.
girIncludes :: Document -> [(String, String)]
girIncludes doc = do
    let includeElems   = topLevelChildsWithLocalName "include" doc
        includeAttribs = elementAttributes <$> includeElems
    Just includeNames    <- M.lookup "name" <$> includeAttribs
    Just includeVersions <- M.lookup "version" <$> includeAttribs
    return (T.unpack includeNames, T.unpack includeVersions)

-- Just helper functions that probably already
-- exist in Data.XML in some form, hiding from me
topLevelChildsWithLocalName :: String -> Document -> [Element]
topLevelChildsWithLocalName n =
    childElemsWithLocalName n . documentRoot

nodeToElement :: Node -> Maybe Element
nodeToElement (NodeElement e) = Just e
nodeToElement _               = Nothing

childElemsWithLocalName :: String -> Element -> [Element]
childElemsWithLocalName n =
    filter localNameMatch . mapMaybe nodeToElement . elementNodes
    where localNameMatch = (== n) . T.unpack . nameLocalName . elementName
