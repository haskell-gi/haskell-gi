{-# CFILES hsgclosure.c #-}
import Gtk hiding (main)
import qualified Gtk as Gtk

import Control.Applicative ((<$>))

import Foreign (nullPtr)

-- A fancy notation for making signal connections easier to read.
(<!>) obj cb = cb obj

main = do
	gtk_init nullPtr nullPtr

	win <- castToWindow <$> windowNew WindowTypeToplevel
        win <!> onWidgetDestroy $ do
                  putStrLn "Closing the program"
                  mainQuit

        grid <- castToGrid <$> gridNew
        orientableSetOrientation grid OrientationVertical
        containerAdd win grid

        label <- castToLabel <$> labelNew "Test"
        widgetShow label
        label <!> onLabelActivateLink $ \uri -> do
                      putStrLn $ uri ++ " clicked."
                      return True -- Link processed, do not open with
                                  -- the browser
        containerAdd grid label

        button <- castToButton <$> buttonNewWithLabel "Click me!"
        button <!> onButtonClicked $
                labelSetMarkup label "This is <a href=\"http://www.gnome.org\">a test</a>"
        containerAdd grid button

	widgetShowAll win
	Gtk.main
