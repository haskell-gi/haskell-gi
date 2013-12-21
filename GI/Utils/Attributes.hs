{-# LANGUAGE GADTs, EmptyDataDecls, ScopedTypeVariables #-}
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
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : experimental
-- Portability : portable
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
  Attr(..),
  WritableAttr,
  NonWritableAttr,
  ConstructibleAttr,
  NonConstructibleAttr,
  ReadableAttr,
  NonReadableAttr,

  -- * Interface for getting, setting and updating attributes
  AttrOp(..),
  JustSetsAttr,
  SetsAndGetsAttr,
  get,
  set
  ) where

import GI.Utils.GValue (GValue(..))

infixr 0 :=,:~,:=>,:~>,::=,::~

-- The idea of the construction below is to be able to enforce at
-- compile time that the read-only etc. properties of attributes are
-- satisfied. In order to do this, we use a GADT on the Attr
-- constructor to tag the type of Attr with one of the types below,
-- and the "get", "set", "new" and the AttrOp constructors specialize
-- the type so that the readability/writability/constructibility
-- constraints are enforced.

-- The case of NonWritableAttr is somewhat special. Attributes which
-- are really non-writable will have that tag set, but writable
-- attributes will be abstract on that type. Functions that don't care
-- whether the attribute is writable or not (such as "get" and "new")
-- will then specialize to NonWritableAttr, which lets them accept
-- both writable and non-writable attrs. "set", which needs the
-- argument to be writable, will specialize to WritableAttr and in
-- this way give an error if a non-writable attr is passed to it.

data ReadableAttr
data NonReadableAttr

data WritableAttr
data NonWritableAttr

data ConstructibleAttr
data NonConstructibleAttr

type Getter o a = o -> IO a
type Setter o b = o -> b -> IO ()
type Constructor b = b -> IO (String, GValue)

-- | The most general version, with a getter, a setter, and a
-- constructor. The type of the attr is parameterized by "a", while
-- "r", "w", "c" parameterize whether the attribute can be read (using
-- "get"), written to (using "set", i.e. after creation of the
-- object), and created (using "new").
data Attr o a b r w c where
    RCAttr :: String -> Getter o a -> Constructor b ->
              Attr o a b ReadableAttr NonWritableAttr ConstructibleAttr
    RWAttr :: String -> Getter o a -> Setter o b -> Constructor b ->
              Attr o a b ReadableAttr w ConstructibleAttr
    ROAttr :: String -> Getter o a ->
              Attr o a () ReadableAttr NonWritableAttr NonConstructibleAttr

    COAttr :: String -> Constructor b ->
              Attr o () b NonReadableAttr NonWritableAttr ConstructibleAttr
    WOAttr :: String -> Setter o b -> Constructor b ->
              Attr o () b NonReadableAttr w ConstructibleAttr

instance Show (Attr o a b r w c) where
  show (RCAttr str _ _)   = str
  show (RWAttr str _ _ _) = str
  show (ROAttr str _)     = str
  show (COAttr str _)     = str
  show (WOAttr str _ _)   = str

data JustSetsAttr
data SetsAndGetsAttr

-- Notice that Writable implies Constructible, so here we just require
-- constructible. "set" will then specialize on w to enforce that the
-- parameter is writable.
data AttrOp s o w where
    -- ^ Assign a value to an attribute
    (:=)  :: Attr o a b r w ConstructibleAttr -> b ->
             AttrOp s o w
    -- ^ Assign the result of an IO action to an attribute
    (:=>) :: Attr o a b r w ConstructibleAttr -> IO b ->
             AttrOp s o w
    -- ^ Apply an update function to an attribute
    (:~)  :: Attr o a b ReadableAttr w ConstructibleAttr -> (a -> b) ->
             AttrOp SetsAndGetsAttr o w
    -- ^ Apply an IO update function to an attribute
    (:~>) :: Attr o a b ReadableAttr w ConstructibleAttr -> (a -> IO b) ->
             AttrOp SetsAndGetsAttr o w
    -- ^ Assign a value to an attribute with the object as an argument
    (::=) :: Attr o a b r w ConstructibleAttr -> (o -> b) ->
             AttrOp SetsAndGetsAttr o w
    -- ^ Apply an update function to an attribute with the object as
    -- an argument
    (::~) :: Attr o a b ReadableAttr w ConstructibleAttr -> (o -> a -> b) ->
             AttrOp SetsAndGetsAttr o w

-- | Set a number of properties for some object.
set :: forall o. o -> [AttrOp SetsAndGetsAttr o WritableAttr] -> IO ()
set obj = mapM_ app
 where
   app :: AttrOp SetsAndGetsAttr o WritableAttr -> IO ()
   app (RWAttr _   _    setter _ :=  x) = setter obj x
   app (WOAttr _        setter _ :=  x) = setter obj x
   app (RWAttr _ getter setter _ :~  f) = getter obj >>= \v -> setter obj (f v)
   app (RWAttr _   _    setter _ :=> x) =                x >>= setter obj
   app (WOAttr _        setter _ :=> x) =                x >>= setter obj
   app (RWAttr _ getter setter _ :~> f) = getter obj >>= f >>= setter obj

   app (RWAttr _   _    setter _ ::= f) = setter obj (f obj)
   app (WOAttr _        setter _ ::= f) = setter obj (f obj)
   app (RWAttr _ getter setter _ ::~ f) = getter obj >>= \v -> setter obj (f obj v)

-- | Get an Attr of an object.
get :: o -> Attr o a b ReadableAttr NonWritableAttr c -> IO a
get o (ROAttr _ getter) = getter o
get o (RWAttr _ getter _ _) = getter o
get o (RCAttr _ getter _) = getter o
