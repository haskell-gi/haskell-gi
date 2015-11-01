{-# LANGUAGE TypeOperators, KindSignatures, DataKinds, PolyKinds,
             TypeFamilies, UndecidableInstances, EmptyDataDecls,
             MultiParamTypeClasses, FlexibleInstances, ConstraintKinds #-}

-- | Helpers for dealing with `GObject`s.

module Data.GI.Base.Overloading
    ( -- * Type level inheritance
      ParentTypes
    , IsDescendantOf

    -- * Looking up attributes in parent types
    , AttributeList
    , ResolveAttribute
    , HasAttribute
    , HasAttr

    -- * Looking up signals in parent types
    , SignalList
    , ResolveSignal
    , HasSignal
    ) where

import GHC.Exts (Constraint)
import GHC.TypeLits

-- | Join two lists.
type family JoinLists (as :: [a]) (bs :: [a]) :: [a] where
    JoinLists '[] bs = bs
    JoinLists (a ': as) bs = a ': JoinLists as bs

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

-- | Check that a type is in the list of `GObjectParents` of another
-- `GObject`-derived type.
type family IsDescendantOf (parent :: *) (descendant :: *) :: Constraint where
    -- Every object is defined to be a descendant of itself.
    IsDescendantOf d d = () ~ ()
    IsDescendantOf p d = CheckForAncestorType d p (ParentTypes d) ~ 'HasAncestor p d

-- | The direct parents of this object: its direct parent type, if any,
-- and the interfaces it implements. The interfaces inherited from
-- parent types can be omitted.
type family ParentTypes a :: [*]

-- | The list of attributes defined for a given type. Each element of
-- the list is a tuple, with the first element of the tuple the name
-- of the attribute, and the second the type encoding the information
-- of the attribute. This type will be an instance of `AttrInfo`.
type family AttributeList a :: [(Symbol, *)]

-- | Datatype returned when the attribute is not found, hopefully making
-- the resulting error messages somewhat clearer.
data UnknownAttribute (msg1 :: Symbol) (s :: Symbol) (msg2 :: Symbol) (o :: *)

-- | Return the type encoding the attribute information for a given
-- type and attribute.
type family ResolveAttribute (s :: Symbol) (o :: *) :: * where
    ResolveAttribute s o = FindElement s (AttributeList o)
                           (UnknownAttribute "Error: could not find attribute" s "for object" o)

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
    HasAttribute attr o = IsElem attr (AttributeList o)
                          'HasAttribute
                          ('DoesNotHaveAttribute "Error: attribute" attr "not found for type" o)
                          ~ 'HasAttribute

-- | A constraint that enforces that the given type has a given attribute.
class HasAttr (attr :: Symbol) (o :: *)
instance HasAttribute attr o => HasAttr attr o

-- | The list of signals defined for a given type. Each element of
-- the list is a tuple, with the first element of the tuple the name
-- of the signal, and the second the type encoding the information of
-- the signal. This type will be an instance of `SignalInfo`.
type family SignalList a :: [(Symbol, *)]

-- | Datatype returned when the signal is not found, hopefully making
-- the resulting error messages somewhat clearer.
data UnknownSignal (msg1 :: Symbol) (s :: Symbol) (msg2 :: Symbol) (o :: *)

-- | Return the type encoding the signal information for a given
-- type and signal.
type family ResolveSignal (s :: Symbol) (o :: *) :: * where
    ResolveSignal s o = FindElement s (SignalList o)
                        (UnknownSignal "Error: could not find signal" s "for object" o)

-- | Isomorphic to Bool, but having some extra debug information.
data SignalCheck s t = HasSignal
                     | DoesNotHaveSignal Symbol s Symbol t

-- | A constraint enforcing that the signal exists for the given
-- object, or one of its ancestors.
type family HasSignal (s :: Symbol) (o :: *) where
    HasSignal s o = IsElem s (SignalList o)
                    'HasSignal
                    ('DoesNotHaveSignal "Error: signal" s "not found for type" o)
                    ~ 'HasSignal
