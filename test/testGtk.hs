{-# CFILES hsgclosure.c #-}
import GLib
import GObject
import Gtk

import Foreign
import Foreign.C

{-
onActivateLink :: GObjectKlass a => a -> GtkLabelActivateLinkCallback -> IO Word32
onActivateLink obj cb = connectSignal obj "activate-link" cb' where
               cb' :: Ptr GtkLabel -> CString -> IO Bool
               cb' _ uri' = do
                   uri <- peekCString uri'
                   cb uri

onClicked :: GObjectKlass a => a -> GtkButtonClickedCallback -> IO Word32
onClicked obj cb = connectSignal obj "clicked" cb' where
          cb' :: Ptr GObject -> IO (Ptr ())
          cb' _ = do
              cb

onDestroy :: GObjectKlass a => a -> GtkWidgetDestroyCallback -> IO Word32
onDestroy obj cb = connectSignal obj "destroy" cb' where
          cb' :: Ptr GtkWidget -> IO (Ptr ())
          cb' _ = do
              cb
-}

-- A fancy notation for making signal connections easier to read.

(<!>) obj cb = cb obj

main = do
	gtk_init nullPtr nullPtr

	win <- gtkWindowNew GtkWindowTypeToplevel
        win <!> onGtkWidgetDestroy $ do
                  putStrLn "Closing the program"
                  gtkMainQuit

        grid <- gtkGridNew
        gtkOrientableSetOrientation (castToGtkGrid grid) GtkOrientationVertical
        gtkContainerAdd (castToGtkContainer win) grid

        label <- gtkLabelNew "Test"
        gtkWidgetShow label
        label <!> onGtkLabelActivateLink $ \uri -> do
                      putStrLn $ uri ++ " clicked."
                      return True -- Link processed, do not open with
                                  -- the browser
        gtkContainerAdd (castToGtkContainer grid) label

        button <- gtkButtonNewWithLabel "Click me!"
        button <!> onGtkButtonClicked $
                gtkLabelSetMarkup (castToGtkLabel label) "This is <a href=\"http://www.gnome.org\">a test</a>"
        gtkContainerAdd (castToGtkContainer grid) button

	gtkWidgetShowAll win
	gtkMain
