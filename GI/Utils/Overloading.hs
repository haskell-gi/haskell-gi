{-# LANGUAGE TypeOperators, KindSignatures, DataKinds, PolyKinds,
             TypeFamilies, UndecidableInstances, EmptyDataDecls,
             MultiParamTypeClasses, FlexibleInstances #-}

-- | Helpers for dealing with `GObject`s.

module GI.Utils.Overloading
    ( -- * Type level inheritance
      ParentTypes
    , IsDescendantOf

    -- * Looking up attributes in parent types
    , AttributeList
    , ResolveAttribute
    , HasAttribute
    , HasAttr
    ) where

import GHC.Exts (Constraint)
import GHC.TypeLits

-- | Join two lists.
type family JoinLists (as :: [a]) (bs :: [a]) :: [a] where

    JoinLists '[] bs = bs
    JoinLists (a ': as) bs = a ': JoinLists as bs

-- | Concat the given lists.
type family ConcatLists (ls :: [[a]]) :: [a] where
    ConcatLists '[] = '[]
    ConcatLists ('[] ': ls) = ConcatLists ls
    ConcatLists ((x ': xs) ': ls) = x ': (ConcatLists (xs ': ls))

-- | Look in the given list of (symbol, tag) tuples for the tag
-- corresponding to the given symbol. If not found return the given
-- default type.
type family FindElement (m :: Symbol) (ms :: [(Symbol, *)]) (d :: *) :: * where
    FindElement m '[] d = d
    FindElement m ('(m, o) ': ms) d = o
    FindElement m ('(m', o) ': ms) d = FindElement m ms d

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

-- | The list of attributes defined for a given type. Each element of
-- the list is a tuple, with the first element of the tuple the name
-- of the attribute, and the second the type encoding the information
-- of the attribute. This type will be an instance of `AttrInfo`.
type family AttributeList a :: [(Symbol, *)]

-- | Get the list of attributes recursively, given a list of types.
type family CollectParentAttributes (ps :: [a]) :: [[(Symbol, *)]] where
    CollectParentAttributes '[] = '[]
    CollectParentAttributes (p ': ps) = CollectAttributes p ':
                                        CollectParentAttributes ps

-- | Collect all attributes of a type, including those defined by
-- the parents.
type family CollectAttributes (a :: *) where
    CollectAttributes a = ConcatLists (AttributeList a ': CollectParentAttributes (ParentTypes a))

-- | Datatype returned when the attribute is not found, hopefully making
-- the resulting error messages somewhat clearer.
data UnknownAttribute (msg1 :: Symbol) (s :: Symbol) (msg2 :: Symbol) (o :: *)

-- | Return the type encoding the attribute information for a given
-- type and attribute.
type family ResolveAttribute (s :: Symbol) (o :: *) :: * where
    ResolveAttribute s o = FindElement s (CollectAttributes o)
                           (UnknownAttribute "Error: could not find attribute" s "for object" o)

-- | Check whether attribute resolution succeeded. This is useful in
-- order to simplify the typechecking error messages in case we try to
-- resolve a non-existent symbol.
type family CheckAttribute (s :: Symbol) (o :: *) :: Constraint where
    CheckAttribute s o = ()

-- | Whether a given type is in the given list. If found, return
-- @success@, otherwise return @failure@.
type family IsElem (e :: Symbol) (es :: [(Symbol, *)]) (success :: k) (failure :: k) :: k where
    IsElem e '[] success failure = failure
    IsElem e ( '(e, t) ': es) success failure = success
    IsElem e ( '(other, t) ': es) s f = IsElem e es s f

-- | Isomorphic to Bool, but having some extra debug information.
data AttributeCheck a t = HasAttribute
                        | DoesNotHaveAttribute Symbol a Symbol t

-- | A constraint imposing that the given object has the given attribute.
type family HasAttribute (attr :: Symbol) (o :: *) where
    HasAttribute attr o = IsElem attr (CollectAttributes o)
                          'HasAttribute
                          ('DoesNotHaveAttribute "Error: attribute" attr "not found for type" o)
                          ~ 'HasAttribute

-- | A constraint that enforces that the given type has a given attribute.
class HasAttr (o :: *) (attr :: Symbol)
instance HasAttribute attr o => HasAttr o attr
