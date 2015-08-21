{-# LANGUAGE TypeOperators, KindSignatures, DataKinds, PolyKinds,
             TypeFamilies, UndecidableInstances #-}

-- | Helpers for dealing with `GObject`s.

module GI.Utils.Overloading
    ( -- * Type level inheritance
      ParentTypes
    , IsDescendantOf
    ) where

import GHC.Exts (Constraint)
import GHC.TypeLits

-- | Result of a ancestor check. Basically a Bool type with a bit of
-- extra info in order to improve typechecker error messages.
data AncestorCheck t a = HasAncestor a t | DoesNotHaveRequiredAncestor Symbol t Symbol a

-- | Check whether a type appears in a list. We specialize the
-- names/types a bit so the error messages are more informative.
type family CheckForAncestorType t (a :: *) (as :: [*]) :: AncestorCheck * * where
    CheckForAncestorType t a '[] =
        'DoesNotHaveRequiredAncestor "Error: Required ancestor" a "not found for type" t
    CheckForAncestorType t a (a ': as) = 'HasAncestor a t
    CheckForAncestorType t a (b ': as) = CheckForAncestorType t a as

-- | Join two lists.
type family JoinLists (as :: [a]) (bs :: [a]) :: [a] where

    JoinLists '[] bs = bs
    JoinLists (a ': as) bs = a ': JoinLists as bs

-- | Collect the list of ancestors of a given set of types, including
-- the types themselves.
type family Ancestors (a :: [*]) :: [*] where
    Ancestors '[] = '[]
    -- We split off this case for better error messages.
    Ancestors (t ': '[]) = t ': Ancestors (ParentTypes t)
    Ancestors (t ': ts) = t ': Ancestors (JoinLists (ParentTypes t) ts)

-- | Check that a type is in the list of `GObjectParents` of another
-- `GObject`-derived type.
type family IsDescendantOf (parent :: *) (descendant :: *) :: Constraint where
    IsDescendantOf p d = CheckForAncestorType d p (Ancestors '[d]) ~ 'HasAncestor p d

-- | The direct parents of this object: its direct parent type, if any,
-- and the interfaces it implements. The interfaces inherited from
-- parent types can be omitted.
type family ParentTypes a :: [*]
