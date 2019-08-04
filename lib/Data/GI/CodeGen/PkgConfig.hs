module Data.GI.CodeGen.PkgConfig
    ( pkgConfigGetVersion
    , tryPkgConfig
    ) where

import Control.Monad (when)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid (First(..), (<>))
#else
import Data.Monoid (First(..))
#endif
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text (Text)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

-- | Try asking pkg-config for the version of a given module, and
-- return the package name together with its version.
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
-- one-to-one some guessing is involved, although in most cases the
-- required information is listed in the GIR file.
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
  let alternatives = case M.lookup lowerName overridenNames of
                       Nothing -> packages ++ [lowerName <> "-" <> version,
                                               lowerName]
                       Just n -> [n <> "-" <> version, n]
      firstJust = getFirst . mconcat . map First
  mapM tryPkgConfig alternatives >>= return . firstJust
