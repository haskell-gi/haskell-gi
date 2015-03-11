{-# LANGUAGE OverloadedStrings, ViewPatterns, TupleSections #-}
module GI.Config
    ( Config(..)
    , parseConfigFile
    ) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text (Text)

import GI.API (Name(..))

data Config = Config {
      -- | Name of the module being generated.
      modName        :: Maybe String,
      -- | Symbols to be renamed.
      renames        :: M.Map String String,
      -- | Prefix for constants in a given namespace, if not given the
      -- "_" string will be used.
      constantPrefix :: M.Map String String,
      -- | Ignored elements of a given API.
      ignoredElems   :: M.Map Name (S.Set String),
      -- | Ignored APIs (all elements in this API will just be discarded).
      ignoredAPIs    :: S.Set Name,
      -- | Structs for which accessors should not be auto-generated.
      sealedStructs  :: S.Set Name
    } deriving Show

-- | Construct the generic config for a module.
defaultConfig :: Config
defaultConfig = Config {
              modName        = Nothing,
              renames        = M.empty,
              constantPrefix = M.empty,
              ignoredElems   = M.empty,
              ignoredAPIs    = S.empty,
              sealedStructs  = S.empty }

-- | There is a sensible notion of zero and addition of Configs,
-- encode this so that we can view the parser as a writer monad of
-- configs.
instance Monoid Config where
    mempty = defaultConfig
    mappend a b = Config {
                         modName = modName a <> modName b,
                         renames = renames a <> renames b,
                         constantPrefix = constantPrefix a <> constantPrefix b,
                         ignoredAPIs = ignoredAPIs a <> ignoredAPIs b,
                         sealedStructs = sealedStructs a <> sealedStructs b,
                         ignoredElems = M.unionWith S.union (ignoredElems a)
                                        (ignoredElems b)
                       }

-- | We have a bit of context (the current namespace), and can fail,
-- encode this in a monad.
type Parser = WriterT Config (StateT (Maybe String) (Except Text)) ()

-- | Parse the given config file (as a set of lines) for a given
-- introspection namespace, filling in the configuration as needed. In
-- case the parsing fails we return a description of the error
-- instead.
parseConfigFile :: [Text] -> Either Text Config
parseConfigFile ls = runExcept $ flip evalStateT Nothing $ execWriterT $
                      mapM parseOneLine (map T.strip ls)

-- | Parse a single line of the config file, modifying the
-- configuration as appropriate.
parseOneLine :: Text -> Parser
-- Empty lines
parseOneLine line | T.null line = return ()
-- Comments
parseOneLine (T.stripPrefix "#" -> Just _) = return ()
parseOneLine (T.stripPrefix "namespace " -> Just ns) =
    (put . Just . T.unpack . T.strip) ns
parseOneLine (T.stripPrefix "ignore " -> Just ign) = get >>= parseIgnore ign
parseOneLine (T.stripPrefix "constantPrefix " -> Just p) = get >>= parseConstP p
parseOneLine (T.stripPrefix "rename " -> Just r) = parseRename r
parseOneLine (T.stripPrefix "seal " -> Just s) = get >>= parseSeal s
parseOneLine l = throwError $ "Could not understand \"" <> l <> "\"."

-- | Ignored elements.
parseIgnore :: Text -> Maybe String -> Parser
parseIgnore _ Nothing =
    throwError "'ignore' requires a namespace to be defined first."
parseIgnore (T.words -> (T.splitOn "." -> [api,elem]):[]) (Just ns) =
    tell $ defaultConfig {ignoredElems = M.singleton (Name ns (T.unpack api))
                                         (S.singleton $ T.unpack elem)}
parseIgnore (T.words -> (T.splitOn "." -> [api]):[]) (Just ns) =
    tell $ defaultConfig {ignoredAPIs = S.singleton (Name ns (T.unpack api))}
parseIgnore ignore _ =
    throwError ("Ignore syntax is of the form \"ignore API.elem\" with '.elem' optional.\nGot \"ignore " <> ignore <> "\" instead.")

-- | Prefix for constants.
parseConstP :: Text -> Maybe String -> Parser
parseConstP _ Nothing = throwError "'constantPrefix' requires a namespace to be defined first. "
parseConstP (T.words -> p:[]) (Just ns) = tell $
    defaultConfig {constantPrefix = M.singleton ns (T.unpack p)}
parseConstP prefix _ =
    throwError ("constantPrefix syntax is of the form \"constantPrefix prefix\".\nGot \"constantPrefix " <> prefix <> "\" instead.")

-- | Symbol renaming.
parseRename :: Text -> Parser
parseRename (T.words -> a:b:[]) = tell $
     defaultConfig {renames = M.singleton (T.unpack a) (T.unpack b)}
parseRename rename =
    throwError ("rename syntax is of the form \"rename old new\".\nGot \"rename " <> rename <> "\" instead.")

-- | Sealed structures.
parseSeal :: Text -> Maybe String -> Parser
parseSeal _ Nothing = throwError "'seal' requires a namespace to be defined first."
parseSeal (T.words -> s:[]) (Just ns) = tell $
    defaultConfig {sealedStructs = S.singleton (Name ns (T.unpack s))}
parseSeal seal _ =
    throwError ("seal syntax is of the form \"seal name\".\nGot \"seal "
                <> seal <> "\" instead.")
