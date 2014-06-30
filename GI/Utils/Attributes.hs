{-# LANGUAGE GADTs, EmptyDataDecls, ScopedTypeVariables, DataKinds,
  KindSignatures, TypeFamilies, MultiParamTypeClasses, ConstraintKinds #-}
{-# OPTIONS_GHC -Wall #-}

-- -*-haskell-*-

--  Based on gtk2hs, with copyright below.

--  GIMP Toolkit (GTK) Attributes interface
--
--  Author : Duncan Coutts
--
--  Created: 21 January 2005
--
--  Copyright (C) 2005 Duncan Coutts
--
--  Partially derived from the hs-fltk and wxHaskell projects which
--  are both under LGPL compatible licenses.
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- |
--
-- Attributes interface
--
-- Attributes of an object can be get and set. Getting the value of an
-- object's attribute is straingtforward. As an example consider a @button@
-- widget and an attribute called @buttonLabel@.
--
-- > value <- get button buttonLabel
--
-- The syntax for setting or updating an attribute is only slightly more
-- complex. At the simplest level it is just:
--
-- > set button [ buttonLabel := value ]
--
-- However as the list notation would indicate, you can set or update multiple
-- attributes of the same object in one go:
--
-- > set button [ buttonLabel := value, buttonFocusOnClick := False ]
--
-- You are not limited to setting the value of an attribute, you can also
-- apply an update function to an attribute's value. That is the function
-- receives the current value of the attribute and returns the new value.
--
-- > set spinButton [ spinButtonValue :~ (+1) ]
--
-- There are other variants of these operators, (see 'AttrOp'). ':=>' and
-- ':~>' and like ':=' and ':~' but operate in the 'IO' monad rather
-- than being pure. There is also '::=' and '::~' which take the object
-- as an extra parameter.
--
-- Attributes can be read only, write only or both read\/write.
--
module GI.Utils.Attributes (
  Attr,
  HasAttr(..),

  AttrSetOpTag(..),
  SettableAndConstructibleAttr,
  ConstructOnlyAttr,
  SetOnlyAttr,
  ReadOnlyAttr,

  -- * Interface for getting, setting and updating attributes
  AttrOp(..),
  AttrOpType(..),
  get,
  set
  ) where

import GI.Utils.GValue (GValue(..))

import GHC.TypeLits
import GHC.Exts (Constraint)

infixr 0 :=,:~,:=>,:~>,::=,::~

-- The idea of the construction below is to be able to enforce at
-- compile time that the read-only etc. properties of attributes are
-- satisfied. In order to do this, we use a GADT on the Attr
-- constructor to tag the type of Attr with one of the types below,
-- and the "get", "set", "new" and the AttrOp constructors specialize
-- the type so that the readability/writability/constructibility
-- constraints are enforced.

data Attr (attr :: Symbol) o

class HasAttr (attr :: Symbol) o where
    type AttrIsReadable attr o :: Bool
    type AttrSettableConstraint attr o :: AttrSetOpTag -> Constraint
    type AttrSetTypeConstraint attr o :: * -> Constraint
    type AttrGetType attr o
    attrLabel :: o -> Attr attr o -> String
    attrGet :: Attr attr o -> o -> IO (AttrGetType attr o)
    attrSet :: (AttrSetTypeConstraint attr o) b =>
               Attr attr o -> o -> b -> IO ()
    attrConstruct :: (AttrSetTypeConstraint attr o) b =>
                     Attr attr o -> b -> IO (String, GValue)

instance HasAttr attr o => Show (Attr attr o) where
  show a = attrLabel (undefined :: o) a

-- The following classes, in combination with the
-- "AttrSettableConstraint" constraint, will enforce that one does not
-- pass a construct only argument to "set", while passing it to "new"
-- is allowed, and disallows "set" and "new" on ReadOnly
-- attributes. The idea is that the corresponding constraint
-- (SettableAndConstructibleAttr, ConstructOnlyAttr, ...) can only
-- succeed when doing the appropriate set/new operation, depending on
-- whether the appropriate instances exist.

data AttrSetOpTag = AttrSet | AttrNew

class SettableAndConstructibleAttr (a :: AttrSetOpTag)
instance SettableAndConstructibleAttr AttrSet
instance SettableAndConstructibleAttr AttrNew

class ConstructOnlyAttr (a :: AttrSetOpTag)
instance ConstructOnlyAttr AttrNew

class SetOnlyAttr (a :: AttrSetOpTag)
instance SetOnlyAttr AttrSet

class ReadOnlyAttr (a :: AttrSetOpTag)

-- We want to accept some AttrOp constructors both for "set" and
-- "new", but some only for "set" (the ones that require the object to
-- exist do not make sense in "new"). We filter things using the
-- following trick: AttrOps that make sense for both "set" and "new"
-- are abstract in AttrOpType, and can specialize to either case, and
-- those specific to "set" are of type "SetOnlyAttrOp". "set" and
-- "new" specialize then as they should.
data AttrOpType = SetOnlyAttrOp | SetAndConstructOp

-- Notice that Writable implies Constructible, so here we just require
-- constructible. "set" will then specialize on w to enforce that the
-- parameter is writable.
data AttrOp (s :: AttrOpType) o (w :: AttrSetOpTag) where
    -- ^ Assign a value to an attribute
    (:=)  :: (HasAttr attr o, (AttrSetTypeConstraint attr o) b,
              (AttrSettableConstraint attr o) w) =>
             Attr attr o -> b -> AttrOp s o w
    -- ^ Assign the result of an IO action to an attribute
    (:=>) :: (HasAttr attr o,
              (AttrSettableConstraint attr o) w,
              (AttrSetTypeConstraint attr o) b) =>
             Attr attr o -> IO b -> AttrOp s o w
    -- ^ Apply an update function to an attribute
    (:~)  :: (HasAttr attr o,
              (AttrSettableConstraint attr o) w,
              (AttrSetTypeConstraint attr o) b,
              a ~ (AttrGetType attr o),
              AttrIsReadable attr o ~ True) =>
             Attr attr o -> (a -> b) -> AttrOp SetOnlyAttrOp o w
    -- ^ Apply an IO update function to an attribute
    (:~>) :: (HasAttr attr o,
              (AttrSettableConstraint attr o) w,
              (AttrSetTypeConstraint attr o) b,
              a ~ (AttrGetType attr o),
              AttrIsReadable attr o ~ True) =>
             Attr attr o -> (a -> IO b) -> AttrOp SetOnlyAttrOp o w
    -- ^ Assign a value to an attribute with the object as an argument
    (::=) :: (HasAttr attr o,
              (AttrSettableConstraint attr o) w,
              (AttrSetTypeConstraint attr o) b) =>
             Attr attr o -> (o -> b) -> AttrOp SetOnlyAttrOp o w
    -- ^ Apply an update function to an attribute with the object as
    -- an argument
    (::~) :: (HasAttr attr o,
              (AttrSettableConstraint attr o) w,
              (AttrSetTypeConstraint attr o) b,
              a ~ (AttrGetType attr o),
              AttrIsReadable attr o ~ True) =>
             Attr attr o -> (o -> a -> b) -> AttrOp SetOnlyAttrOp o w

-- | Set a number of properties for some object.
set :: forall o. o -> [AttrOp SetOnlyAttrOp o AttrSet] -> IO ()
set obj = mapM_ app
 where
   app :: AttrOp SetOnlyAttrOp o AttrSet -> IO ()
   app (attr :=  x) = attrSet attr obj x
   app (attr :=> x) = x >>= attrSet attr obj
   app (attr :~  f) = attrGet attr obj >>= \v -> attrSet attr obj (f v)
   app (attr :~> f) = attrGet attr obj >>= f >>= attrSet attr obj
   app (attr ::= f) = attrSet attr obj (f obj)
   app (attr ::~ f) = attrGet attr obj >>= \v -> attrSet attr obj (f obj v)

-- | Get an Attr of an object.
get :: (HasAttr attr o, AttrIsReadable attr o ~ True) =>
        o -> Attr attr o -> IO (AttrGetType attr o)
get = flip attrGet
