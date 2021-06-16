{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}



module Main where

import Data.Maybe
import qualified Data.Text as T
import System.FilePath
import Paths_rsvg

import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.Rsvg as Rsvg
import qualified GI.Cairo as Cairo
import qualified GI.Cairo.Render as Cairo
import qualified GI.Cairo.Render.Connector as Cairo
import Data.GI.Base



main :: IO ()
main = do
  Gtk.init Nothing

  -- Read the SVG file and get its size.
  dataDir <- getDataDir
  drawing <- fromMaybe (error "drawing.svg has gone missing.") <$>
      Rsvg.handleNewFromFile (T.pack $ dataDir </> "drawing.svg")
  (_, drawW, _, drawH, _, _) <- Rsvg.handleGetIntrinsicDimensions drawing
    -- We know this SVG has width and height because it ships with the package. Normally
    -- you would check. We also know these are mm,
  drawW1 <- Rsvg.get drawW #length
  drawH1 <- Rsvg.get drawH #length

  -- Create a window we can draw on with Cairo.
  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.windowSetPosition window Gtk.WindowPositionCenterAlways

  Gtk.widgetSetAppPaintable window True

  Gtk.windowSetDefaultSize window (ceiling drawH1) (ceiling drawW1)

  geometry <- Gdk.newZeroGeometry
  Gdk.setGeometryMaxWidth  geometry $ ceiling drawW1
  Gdk.setGeometryMaxHeight geometry $ ceiling drawH1
  Gdk.setGeometryMinWidth  geometry 32
  Gdk.setGeometryMinHeight geometry 32

  Gtk.windowSetGeometryHints window (Just window) (Just geometry) []

  Gtk.onWidgetKeyPressEvent window $ \keyPressInfo -> do
    keyVal <- Gdk.getEventKeyKeyval keyPressInfo
    keyName <- fromMaybe T.empty <$> Gdk.keyvalName keyVal
    case keyName of
      "Escape" -> do Gtk.mainQuit
                     return True
      _        -> return False


  canvas <- Gtk.drawingAreaNew
  Gtk.containerAdd window canvas

  Gtk.setWindowDecorated window True
  Gtk.setWindowResizable window True
  Gtk.setWindowTitle window "Cairo SVG Rendering Example"

  Gtk.onWidgetDraw canvas $ Cairo.renderWithContext $ drawCanvasHandler drawing canvas

  Gtk.widgetShowAll window
  Gtk.main


-- | Paint the drawing referred to by the Handle on to the DrawingArea using the Context.
-- The drawing is resized to fit the DrawingArea.
drawCanvasHandler :: Rsvg.Handle -> Gtk.DrawingArea -> Cairo.Render Bool
drawCanvasHandler drawing canvas ctx = do
   width  <- Gtk.widgetGetAllocatedWidth  canvas
   height <- Gtk.widgetGetAllocatedHeight canvas
   rect <- Gtk.new Rsvg.Rectangle
         [#height := fromIntegral height, #width := fromIntegral width, #x := 0, #y := 0]
   Cairo.setSourceRGB 1 1 1  -- White
   Cairo.paint  -- Blank white background.
   ctx <- getContext
   Rsvg.handleRenderDocument drawing ctx rect
   return True
