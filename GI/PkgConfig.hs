{-# LANGUAGE OverloadedStrings #-}
module GI.PkgConfig
    ( pkgConfigGetVersion
    ) where

import Control.Monad (when)
import Data.Monoid (First(..), (<>))
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat)
#endif
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text (Text)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

-- | Known mappings from (lowercased) gobject-introspection namespaces
-- to package names known to pkg-config.
builtinMappings :: M.Map Text Text
builtinMappings = M.fromList [("gtk", "gtk+")
                             ,("gdkpixbuf", "gdk-pixbuf")]

-- | Try asking pkg-config for the version of a given module.
tryPkgConfig :: Text -> IO (Maybe (Text, Text))
tryPkgConfig pkgName = do
  (exitcode, stdout, _) <-
      readProcessWithExitCode "pkg-config" ["--modversion", T.unpack pkgName] ""
  case exitcode of
    ExitSuccess -> case lines stdout of
                     [v] -> return (Just (pkgName, T.pack v))
                     _ -> return Nothing
    ExitFailure _ -> return Nothing

-- | Get the pkg-config name and associated installed version of a given
-- gobject-introspection namespace. Since the mapping is not
-- one-to-one some guessing is involved, and in some cases we need to
-- resort to mapping tables.
pkgConfigGetVersion :: Text     -- name
                    -> Text     -- version
                    -> [Text]   -- known package names
                    -> Bool     -- verbose
                    -> M.Map Text Text -- suggested overrides
                    -> IO (Maybe (Text, Text))
pkgConfigGetVersion name version packages verbose overridenNames = do
  let lowerName = T.toLower name
  when verbose $
           putStrLn $ T.unpack ("Querying pkg-config for " <> name <>
                              " version " <> version)
  let knownMappings = M.union builtinMappings overridenNames
      alternatives = case M.lookup lowerName knownMappings of
                       Nothing -> packages ++ [lowerName <> "-" <> version,
                                               lowerName]
                       Just n -> [n <> "-" <> version, n]
      firstJust = getFirst . mconcat . map First
  mapM tryPkgConfig alternatives >>= return . firstJust
