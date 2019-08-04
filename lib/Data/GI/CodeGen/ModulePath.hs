{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Abstract representation for paths into modules.
module Data.GI.CodeGen.ModulePath
  ( ModulePath(..)
  , toModulePath
  , (/.)
  , dotModulePath
  ) where

#if !MIN_VERSION_base(4,13,0)
import Data.Monoid (Monoid(..), (<>))
#endif
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Semigroup as Sem
import Data.Text (Text)

import Data.GI.CodeGen.Util (ucFirst)

-- | A path to a module.
newtype ModulePath = ModulePath { modulePathToList :: [Text] }
  deriving (Sem.Semigroup, Monoid, Eq, Show, Ord)

-- | Construct a `ModulePath` from a `String`.
instance IsString ModulePath where
  fromString = toModulePath . T.pack

-- | Construct a path into the given GIR namespace. The given `Text`
-- will be split along ".".
--
-- === __Examples__
-- >>> dotModulePath (toModulePath "Foo")
-- "Foo"
--
-- >>> dotModulePath ("Foo" <> toModulePath "Bar.Baz")
-- "Foo.Bar.Baz"
--
-- >>> dotModulePath ("Foo" <> toModulePath "bar.baz")
-- "Foo.Bar.Baz"
toModulePath :: Text -> ModulePath
toModulePath p = ModulePath (map ucFirst (T.split (== '.') p))

-- | Turn a module path into the corresponding dotted string. Note
-- that the implementation ensures that the module names start with a
-- capital letter.
--
-- === __Examples__
-- >>> dotModulePath ("Foo" /. "Bar" /. "Baz")
-- "Foo.Bar.Baz"
--
-- >>> dotModulePath ("foo" /. "bar" /. "baz")
-- "Foo.Bar.Baz"
dotModulePath :: ModulePath -> Text
dotModulePath (ModulePath mp) = T.intercalate "." mp

-- | Append the given component to the given module path.
--
-- === __Examples__
-- >>> dotModulePath ("Foo" /. "Bar")
-- "Foo.Bar"
(/.) :: ModulePath -> Text -> ModulePath
(/.) mp p = mp <> toModulePath p
