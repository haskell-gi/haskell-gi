{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Helper functions for working with 'Widget's.

module Data.GI.Gtk.Widget
    ( printWidgetTree
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (for_)
import Data.GI.Base.GObject (gtypeFromInstance)
import GI.Gtk.Objects.Widget (IsWidget, Widget, toWidget)
import GI.Gtk (Container(Container), castTo, containerGetChildren, gtypeName, managedForeignPtr, toManagedPtr)

-- | Print out a tree of decendents for a given GTK 'Widget'.  This function is
-- mainly to help with debugging.
--
-- This function outputs a tree of 'Widget's like the following:
--
-- > GtkApplicationWindow  0x00000000068de2a0
-- >   GtkMenuBar  0x0000000006c661d0
-- >     GtkModelMenuItem  0x0000000006c72b00
-- >       GtkAccelLabel  0x0000000006c73b60
-- >     GtkModelMenuItem  0x0000000006c723c0
-- >       GtkAccelLabel  0x0000000006c733a0
-- >   GtkNotebook  0x0000000006b0a200
-- >     GtkPaned  0x0000000006b073c0
-- >       GtkScrolledWindow  0x0000000006b0c7c0
-- >         VteTerminal  0x00000000068af4a0
-- >       GtkScrolledWindow  0x0000000006b0c470
-- >         VteTerminal  0x00000000068af370
--
-- Note that you may also be interested in
-- <https://wiki.gnome.org/Projects/GTK/Inspector GTKInspector>, which is a
-- built-in interactive debugger for GTK applications.
printWidgetTree :: forall m a. (MonadIO m, IsWidget a) => a -> m ()
printWidgetTree widget_ = do
  widget <- toWidget widget_
  go "" widget
  where
    go :: String -> Widget -> m ()
    go indent w = do
      type_ <- liftIO $ gtypeFromInstance w
      name <- liftIO $ gtypeName type_
      let ptr = managedForeignPtr . toManagedPtr $ w
      liftIO $ putStrLn $ indent <> name <> "  " <> show ptr
      maybeContainer <- liftIO $ castTo Container w
      for_ maybeContainer $ \container -> do
        children <- containerGetChildren container
        for_ children $ \child -> do
          go ("  " <> indent) child
