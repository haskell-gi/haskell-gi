{-# LANGUAGE TemplateHaskell #-}
-- | Project information to include in generated bindings, should be
-- kept in sync with haskell-gi.cabal
module Data.GI.CodeGen.ProjectInfo
    ( homepage
    , authors
    , license
    , licenseText
    , category
    , maintainers
    , defaultExtensions
    , otherExtensions
    , ghcOptions
    , defaultLanguage
    , standardDeps
    ) where

import Data.FileEmbed (embedStringFile)
import Data.Text (Text)

homepage :: Text
homepage = "https://github.com/haskell-gi/haskell-gi"

authors :: Text
authors = "Will Thompson, Iñaki García Etxebarria and Jonas Platte"

maintainers :: Text
maintainers = "Iñaki García Etxebarria (garetxe@gmail.com)"

license :: Text
license = "LGPL-2.1"

licenseText :: Text
licenseText = $(embedStringFile "LICENSE")

-- | Default list of extensions to turn on when compiling the
-- generated code.
defaultExtensions :: [Text]
defaultExtensions = ["NoImplicitPrelude", "ScopedTypeVariables", "CPP",
                     "OverloadedStrings", "NegativeLiterals", "ConstraintKinds",
                     "TypeFamilies", "MultiParamTypeClasses", "KindSignatures",
                     "FlexibleInstances", "UndecidableInstances", "DataKinds",
                     "FlexibleContexts"]

-- | Extensions that will be used in some modules, but we do not wish
-- to turn on by default.
otherExtensions :: [Text]
otherExtensions = ["PatternSynonyms", "ViewPatterns"]

-- | Default options for GHC when compiling generated code.
ghcOptions :: [Text]
ghcOptions = ["-fno-warn-unused-imports", "-fno-warn-warnings-deprecations"]

-- | Default version of the report to use.
defaultLanguage :: Text
defaultLanguage = "Haskell2010"

-- | List of dependencies for all bindings. Notice that base is not
-- included here, since not all bindings use the same base
-- version. haskell-gi and haskell-gi-base are not included either,
-- since the versions to use may change depending on whether we are
-- using old style or new style bindings.
standardDeps :: [Text]
standardDeps = ["bytestring >= 0.10 && < 1",
                "containers >= 0.5 && < 1",
                "text >= 1.0 && < 2",
                "transformers >= 0.4 && < 1"]

-- | Under which category in hackage should the generated bindings be listed.
category :: Text
category = "Bindings"
