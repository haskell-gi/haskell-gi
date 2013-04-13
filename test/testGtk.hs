{-# CFILES hsgclosure.c #-}
import Gtk hiding (main)
import qualified Gtk as Gtk

import Foreign (nullPtr)

-- A fancy notation for making signal connections easier to read.
(<!>) obj cb = cb obj

main = do
	gtk_init nullPtr nullPtr

	win <- windowNew WindowTypeToplevel
        win <!> onWidgetDestroy $ do
                  putStrLn "Closing the program"
                  mainQuit

        grid <- gridNew
        orientableSetOrientation (castToGrid grid) OrientationVertical
        containerAdd (castToContainer win) grid

        label <- labelNew "Test"
        widgetShow label
        label <!> onLabelActivateLink $ \uri -> do
                      putStrLn $ uri ++ " clicked."
                      return True -- Link processed, do not open with
                                  -- the browser
        containerAdd (castToContainer grid) label

        button <- buttonNewWithLabel "Click me!"
        button <!> onButtonClicked $
                labelSetMarkup (castToLabel label) "This is <a href=\"http://www.gnome.org\">a test</a>"
        containerAdd (castToContainer grid) button

	widgetShowAll win
	Gtk.main
