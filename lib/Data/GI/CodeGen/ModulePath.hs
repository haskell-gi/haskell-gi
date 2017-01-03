-- | Abstract representation for paths into modules.
module Data.GI.CodeGen.ModulePath
  ( ModulePath(..)
  , toModulePath
  , (/.)
  , dotModulePath
  ) where

import Data.Monoid (Monoid(..), (<>))
import Data.String (IsString(..))
import qualified Data.Text as T
import Data.Text (Text)

import Data.GI.CodeGen.Util (ucFirst)

-- | A path to a module.
newtype ModulePath = ModulePath { modulePathToList :: [Text] }
  deriving (Eq, Show, Ord)

-- | Construct a `ModulePath` from a `String`.
instance IsString ModulePath where
  fromString = toModulePath . T.pack

-- | `ModulePath`s are `Monoid`s.
instance Monoid ModulePath where
  mempty = ModulePath []
  mappend (ModulePath as) (ModulePath bs) = ModulePath (as <> bs)

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
