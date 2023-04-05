{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.GI.Base (AttrOp ((:=)), new, on)
import Data.GI.Gtk.Threading (postGUIASync)
import Data.IORef (newIORef, readIORef, writeIORef)
import GI.Cairo.Render qualified as R
import GI.Cairo.Render.Connector qualified as RC
import GI.Gdk qualified as Gdk
import GI.Gtk qualified as Gtk

render :: (Double, Double) -> Double -> R.Render ()
render (xcenter, ycenter) scale = do
    R.save
    R.translate xcenter ycenter
    R.scale scale scale
    R.setSourceRGBA 0 0 0 1
    R.rectangle (-100) (-100) 200 200
    R.fill
    R.restore

main :: IO ()
main = do
    _ <- Gtk.init Nothing
    ref <- newIORef ((0, 0), 1.0)
    mainWindow <- new Gtk.Window [#type := Gtk.WindowTypeToplevel]
    _ <- mainWindow `on` #destroy $ Gtk.mainQuit
    drawArea <- new Gtk.DrawingArea []
    _ <- drawArea
        `on` #draw
        $ RC.renderWithContext
        $ do
            ((xcenter, ycenter), scale) <- R.liftIO $ readIORef ref
            render (xcenter, ycenter) scale
            pure True
    #addEvents drawArea [Gdk.EventMaskButtonPressMask, Gdk.EventMaskTouchpadGestureMask]

    #setDefaultSize mainWindow 640 400
    gzoom <- Gtk.gestureZoomNew drawArea
    _ <- Gtk.onGestureZoomScaleChanged gzoom $ \scale -> do
        (_, xcenter, ycenter) <- #getBoundingBoxCenter gzoom
        writeIORef ref ((xcenter, ycenter), scale)
        postGUIASync $
            #queueDraw drawArea
    #setPropagationPhase gzoom Gtk.PropagationPhaseBubble
    #add mainWindow drawArea
    #showAll mainWindow
    Gtk.main
