{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module ProjectInfo
    ( ProjectInfo(..)
    , prettyConfig
    ) where

import GHC.Generics
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP

import Data.Map (Map)
import Data.Text (Text)

data ProjectInfo = ProjectInfo {
      name              :: Text -- ^ Name of the haskell package
    , version           :: Text -- ^ Its version
    , author            :: Maybe Text -- ^ Auhor. If not specified the
                                      -- current maintainer (from
                                      -- Data.GI.CodeGen.ProjectInfo)
                                      -- will be used.
    , synopsis          :: Text
    , description       :: Text
    , pkgconfigDepends  :: Text
    , giDepends         :: [Text] -- ^ dependencies on other
                                  -- haskell-gi generated modules.
    , baseVersion       :: Text -- ^ Constraint on base for this module.

    , girName           :: Text         -- ^ GIR file (without version)
    , girVersion        :: Text         -- ^ Its version
    , girOverrides      :: Maybe FilePath   -- ^ Possibly an overrides file
    , distributionPackages :: Map Text [Text] -- ^ Package lists for distributions
    , compat            :: Maybe Text
    } deriving (Show, Generic)

instance A.FromJSON ProjectInfo
instance A.ToJSON ProjectInfo

prettyConfig :: AP.Config
prettyConfig = AP.defConfig { AP.confCompare = AP.keyOrder
                              ["name", "version", "description", "synopsis",
                               "author", "girName", "girVersion",
                               "girOverrides", "pkgConfig", "depends",
                               "baseVersion",
                               "distributionPackages"]}
