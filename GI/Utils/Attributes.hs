{-# LANGUAGE GADTs, EmptyDataDecls #-}
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
  RWCAttr(..),

  -- * Interface for getting, setting and updating attributes
  AttrOp(..),
  JustSetsAttr,
  SetsAndGetsAttr,
  get,
  set
  ) where

import GI.Utils.BasicTypes (GValuePtr)

infixr 0 :=,:~,:=>,:~>,::=,::~

-- | The most general version, with a getter, a setter, and a constructor.
data RWCAttr o a b =
    Attr String !(o -> IO a) !(o -> b -> IO ()) !(b -> IO (String, GValuePtr))

instance Show (RWCAttr o a b) where
  show (Attr str _ _ _) = str

-- The idea of the construction below is to be able to check at
-- compile time that "new" is called only with constructors that do
-- not require a read (AttrSetOp). "set", on the other hand, can work
-- also with constructors that require a read (AttrGetSetOp). Using
-- the GADT and specializing on the signature of the receiving
-- function allows us to use the same syntax on constructors for both
-- "set" and "new", but with checking at the type level that we don't
-- try to call "new" with a constructor that is not just setting an
-- argument.
data JustSetsAttr
data SetsAndGetsAttr

data AttrOp s o where
    -- ^ Assign a value to an attribute
    (:=)  :: RWCAttr o a b -> b -> AttrOp s o
    -- ^ Assign the result of an IO action to an attribute
    (:=>) :: RWCAttr o a b -> IO b -> AttrOp s o
    -- ^ Apply an update function to an attribute
    (:~)  :: RWCAttr o a b -> (a -> b) -> AttrOp SetsAndGetsAttr o
    -- ^ Apply an IO update function to an attribute
    (:~>) :: RWCAttr o a b -> (a -> IO b) -> AttrOp SetsAndGetsAttr o
    -- ^ Assign a value to an attribute with the object as an argument
    (::=) :: RWCAttr o a b -> (o -> b) -> AttrOp SetsAndGetsAttr o
    -- ^ Apply an update function to an attribute with the object as
    -- an argument
    (::~) :: RWCAttr o a b -> (o -> a -> b) -> AttrOp SetsAndGetsAttr o

-- | Set a number of properties for some object.
set :: o -> [AttrOp SetsAndGetsAttr o] -> IO ()
set obj = mapM_ app
 where
   app (Attr _   _    setter _ :=  x) = setter obj x
   app (Attr _ getter setter _ :~  f) = getter obj >>= \v -> setter obj (f v)
   app (Attr _   _    setter _ :=> x) =                x >>= setter obj
   app (Attr _ getter setter _ :~> f) = getter obj >>= f >>= setter obj

   app (Attr _   _    setter _ ::= f) = setter obj (f obj)
   app (Attr _ getter setter _ ::~ f) = getter obj >>= \v -> setter obj (f obj v)

-- | Get an Attr of an object.
get :: o -> RWCAttr o a b -> IO a
get o (Attr _ getter _ _) = getter o
