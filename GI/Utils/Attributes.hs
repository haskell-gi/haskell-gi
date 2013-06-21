{-# LANGUAGE GADTs, EmptyDataDecls, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- -*-haskell-*-

--  Based on gtk2hs, with copyright as below.

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
  ConstructibleAttr,
  ReadOnlyAttr,
  ReadableAttr,
  UnreadableAttr,

  -- * Interface for getting, setting and updating attributes
  AttrOp(..),
  JustSetsAttr,
  SetsAndGetsAttr,
  get,
  set
  ) where

import GI.Utils.BasicTypes (GValuePtr)

infixr 0 :=,:~,:=>,:~>,::=,::~

data WritableAttr
data ConstructibleAttr
data ReadOnlyAttr

data ReadableAttr
data UnreadableAttr

type Getter o a = o -> IO a
type Setter o b = o -> b -> IO ()
type Constructor b = b -> IO (String, GValuePtr)

-- | The most general version, with a getter, a setter, and a constructor.
data Attr o a b r w where
    RCAttr :: String -> Getter o a -> Constructor b ->
              Attr o a b ReadableAttr ConstructibleAttr
    RWAttr :: String -> Getter o a -> Setter o b -> Constructor b ->
              Attr o a b ReadableAttr w
    ROAttr :: String -> Getter o a ->
              Attr o a () ReadableAttr ReadOnlyAttr

    COAttr :: String -> Constructor b ->
              Attr o () b UnreadableAttr ConstructibleAttr
    WOAttr :: String -> Setter o b -> Constructor b ->
              Attr o () b UnreadableAttr w

instance Show (Attr o a b r w) where
  show (RCAttr str _ _)   = str
  show (RWAttr str _ _ _) = str
  show (ROAttr str _)     = str
  show (COAttr str _)     = str
  show (WOAttr str _ _)   = str

-- The idea of the construction below is to be able to check at
-- compile time that "new" is called only with constructors that do
-- not require a read (AttrSetOp). "set", on the other hand, can work
-- also with constructors that require a read (AttrGetSetOp). Using
-- the GADT and specializing on the signature of the receiving
-- function allows us to use the same syntax on constructors for both
-- "set" and "new", but with checking at the type level that we don't
-- try to call "new" with a constructor that is not just setting an
-- argument. Compile-time checking of readability/writability of
-- read/write properties is done using the same idea.
data JustSetsAttr
data SetsAndGetsAttr

data AttrOp s o w where
    -- ^ Assign a value to an attribute
    (:=)  :: Attr o a b r w -> b -> AttrOp s o w
    -- ^ Assign the result of an IO action to an attribute
    (:=>) :: Attr o a b r w -> IO b -> AttrOp s o w
    -- ^ Apply an update function to an attribute
    (:~)  :: Attr o a b ReadableAttr w -> (a -> b) ->
             AttrOp SetsAndGetsAttr o w
    -- ^ Apply an IO update function to an attribute
    (:~>) :: Attr o a b ReadableAttr w -> (a -> IO b) ->
             AttrOp SetsAndGetsAttr o w
    -- ^ Assign a value to an attribute with the object as an argument
    (::=) :: Attr o a b r w -> (o -> b) ->
             AttrOp SetsAndGetsAttr o w
    -- ^ Apply an update function to an attribute with the object as
    -- an argument
    (::~) :: Attr o a b ReadableAttr w -> (o -> a -> b) ->
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
get :: o -> Attr o a b ReadableAttr w -> IO a
get o (ROAttr _ getter) = getter o
get o (RWAttr _ getter _ _) = getter o
get o (RCAttr _ getter _) = getter o
