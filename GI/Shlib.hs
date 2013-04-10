
module GI.Shlib where

import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)
import System.Process (readProcess)

import qualified Data.Set as S

import GI.API (API(..), Interface(..), Function(..), Object(..))
import GI.Internal.Typelib
import GI.Util (split)

libraryDefinedSymbols path = do
    s <- readProcess "nm" ["-D", "-fp", path] ""
    return $ catMaybes $ flip map (lines s) $ \line ->
      let words = split ' ' line
      in case words of
          (sym : "T" : _) -> Just sym
          _ -> Nothing

libraryFullPath name = do
    strip <$> readProcess "gcc" ["-print-file-name=" ++ name] ""

    where strip = reverse . dropSpaces . reverse . dropSpaces
          dropSpaces = dropWhile (`elem` "\n ")

typelibSymbols name = do
    shlibs <- getSharedLibraries name

    let shlibPaths = case shlibs of
          Nothing -> error $ "no shared libraries for " ++ name
          Just s -> s

    foldl S.union S.empty <$>
        mapM (\name -> S.fromList <$> (libraryDefinedSymbols =<< libraryFullPath name)) shlibPaths

-- Filter out API items that refer to symbols not in the given set.
strip :: S.Set String -> API -> Maybe API
strip defined api =
    case api of
      a@(APIFunction f) -> if filterFunc f then Just a else Nothing
      APIObject o@(Object { objMethods = methods }) ->
          Just $ APIObject (o { objMethods = filter (filterFunc . snd) methods })
      APIInterface i@(Interface { ifMethods = methods }) ->
          Just $ APIInterface (i { ifMethods = filter (filterFunc . snd) methods })
      a -> Just a
    where
      filterFunc (Function { fnSymbol = sym }) = sym `S.member` defined

