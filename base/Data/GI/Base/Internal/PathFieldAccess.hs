{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Support for creating lenses from overloaded labels of the type
-- @#fieldName@, or @#"fieldName.subfield"@.

module Data.GI.Base.Internal.PathFieldAccess
  ( Components, PathFieldAccess(..)) where

import qualified GHC.TypeLits as TL
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Optics.Core as O
import qualified Optics.Internal.Generic as OG

-- The Char class was introduced in GHC 9.2 (base 4.16), if this is
-- not available we fall back to an implementation that does not allow
-- for nested access. Note that overloaded labels of the form
-- #"record.subrecord" only became valid syntax in 9.4, so this is
-- only really useful from that version.
#if MIN_VERSION_base(4,16,0)
type family AppendChar (s :: TL.Symbol) (c :: Char) where
  AppendChar s c = TL.AppendSymbol s (TL.ConsSymbol c "")

type family DoSplit (c :: Char) (acc :: TL.Symbol) (uncons :: Maybe (Char, TL.Symbol)) where
  DoSplit _ acc Nothing = '[acc]
  DoSplit c acc (Just '(c, rest)) = acc : DoSplit c "" (TL.UnconsSymbol rest)
  DoSplit c acc (Just '(k, rest)) = DoSplit c (AppendChar acc k) (TL.UnconsSymbol rest)

type family SplitByChar (c :: Char) (s :: TL.Symbol) :: [TL.Symbol] where
  SplitByChar c s = DoSplit c "" (TL.UnconsSymbol s)

type family Components (s :: TL.Symbol) :: [TL.Symbol] where
  Components s = SplitByChar '.' s
#else
type family Components (s :: TL.Symbol) :: [TL.Symbol] where
  Components s = '[s]
#endif

-- | Check that the given symbol is not the empty string. If it is,
-- raise a TypeError with the given msg.
type family NonEmpty (s :: TL.Symbol) (msg :: TL.ErrorMessage) :: Constraint where
  NonEmpty "" msg = TL.TypeError msg
  NonEmpty _ _ = ()

-- | Create a lens for the given path, and return it together with the
-- path split into components.
class PathFieldAccess (path :: [TL.Symbol]) (model :: Type) (val :: Type) | path model -> val where
  pathFieldAccess :: Proxy path -> Proxy model -> (O.Lens' model val, [T.Text])

instance {-# OVERLAPPING #-}
  (OG.GFieldImpl fieldName model model val val,
   NonEmpty fieldName (TL.Text "Field names cannot be empty"),
   TL.KnownSymbol fieldName) =>
  PathFieldAccess (fieldName : '[]) model val where
  pathFieldAccess _ _ = (OG.gfieldImpl @fieldName @model @model @val @val,
                         [T.pack $ TL.symbolVal (Proxy @fieldName)])

instance (OG.GFieldImpl fieldName model model val val,
          TL.KnownSymbol fieldName,
          PathFieldAccess rest val inner) =>
         PathFieldAccess (fieldName : rest) model inner where
  pathFieldAccess _ _ =
    let (innerLens, innerPath) = pathFieldAccess (Proxy @rest) (Proxy @val)
        outerLens = OG.gfieldImpl @fieldName @model @model @val @val
        outerName = T.pack $ TL.symbolVal (Proxy @fieldName)
    in (outerLens O.% innerLens, outerName : innerPath)

