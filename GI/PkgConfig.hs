module GI.PkgConfig
    ( pkgConfigGetVersion
    ) where

import Control.Monad (when)
import Data.Char (toLower)
import Data.Monoid (First(..))
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat)
#endif
import qualified Data.Map.Strict as M
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import GI.Internal.Typelib (getVersion, load)

-- | Known mappings from (lowercased) gobject-introspection namespaces
-- to package names known to pkg-config.
builtinMappings :: M.Map String String
builtinMappings = M.fromList [("gtk", "gtk+")
                             ,("gdkpixbuf", "gdk-pixbuf")]

-- | Try asking pkg-config for the version of a given module.
tryPkgConfig :: String -> IO (Maybe (String, String))
tryPkgConfig pkgName = do
  (exitcode, stdout, _) <-
      readProcessWithExitCode "pkg-config" ["--modversion", pkgName] ""
  case exitcode of
    ExitSuccess -> case takeWhile (/='\n') stdout of
                     [] -> return Nothing
                     v -> return (Just (pkgName, v))
    ExitFailure _ -> return Nothing

-- | Get the pkg-config name and associated installed version of a given
-- gobject-introspection namespace. Since the mapping is not
-- one-to-one some guessing is involved, and in some cases we need to
-- resort to mapping tables.
pkgConfigGetVersion :: String -> Bool -> M.Map String String ->
                       IO (Maybe (String, String))
pkgConfigGetVersion name verbose overridenNames = do
  let lowerName = map toLower name
  giVersion <- load name Nothing verbose >>= getVersion
  when verbose $
           putStrLn ("Querying pkg-config for " ++ name ++
                     " version " ++ giVersion)
  let knownMappings = M.union builtinMappings overridenNames
      alternatives = case M.lookup lowerName knownMappings of
                       Nothing -> [lowerName ++ "-" ++ giVersion, lowerName]
                       Just n -> [n ++ "-" ++ giVersion, n]
      firstJust = getFirst . mconcat . map First
  mapM tryPkgConfig alternatives >>= return . firstJust
