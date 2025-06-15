{-# Language DataKinds #-}
module Declarative.Gtk
  (module Declarative.Gtk.Box,
   module Declarative.Gtk.HListStore,
   module Declarative.Gtk.ModelViewHandler,

   module Data.GI.Base,

   widget
  ) where

import Declarative.Gtk.Box
import Declarative.Gtk.HListStore
import Declarative.Gtk.ModelViewHandler

import qualified Data.GI.Base as B
import Data.GI.Base (new, get, SignalProxy(PropertySet), AttrOp(..))
import Data.GI.Base.Attributes (AttrOpTag(..))
import Data.GI.Base.GObject (constructGObject)
import qualified Data.GI.Base.Overloading as B.Overloading
import qualified GI.Gtk as Gtk

-- | A convenience constructor: like `Data.GI.Base.new`, but casts the
-- result to `Gtk.Widget` (assuming that the type being constructed
-- can be safely cast to `Gtk.Widget`).
widget :: (B.GObject a, B.Overloading.IsDescendantOf Gtk.Widget a) =>
          (B.ManagedPtr a -> a) -> [AttrOp a AttrConstruct] -> IO Gtk.Widget
widget constructor attrs = Gtk.toWidget =<< constructGObject constructor attrs
