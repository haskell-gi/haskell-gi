-- | Project information to include in generated bindings, should be
-- kept in sync with GObject-Introspection.cabal
module GI.ProjectInfo
    ( homepage
    , authors
    , license
    , maintainers
    ) where

homepage :: String
homepage = "https://github.com/garetxe/haskell-gi"

authors :: String
authors = "Will Thompson, Iñaki García Etxebarria and Jonas Platte"

maintainers :: String
maintainers = "Iñaki García Etxebarria (garetxe@gmail.com)"

license :: String
license = "LGPL-3"
