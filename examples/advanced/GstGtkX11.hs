{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

{-
  A minimal example of embedding a Gstreamer video into a GTK/X11 window.

  The 3-Clause BSD License

  Copyright 2017 David Lettier

  Redistribution and use in source and binary forms,
  with or without modification, are permitted provided
  that the following conditions are met:

  1. Redistributions of source code must retain the above
  copyright notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above
  copyright notice, this list of conditions and the following
  disclaimer in the documentation and/or other materials provided
  with the distribution.

  3. Neither the name of the copyright holder nor the names of its
  contributors may be used to endorse or promote products derived
  from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
  COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
  OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
  EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

module Main where

import Foreign.C.Types
import Data.Maybe
import Data.GI.Base.Properties
import qualified Data.GI.Base.Overloading as O
import qualified GI.Gtk
import GI.Gst
import GI.GstVideo
import GI.GdkX11

-- Playbin implements the Gstreamer VideoOverlay interface, but there
-- is no Playbin type in the introspection data, so we create a
-- newtype and make it descent from VideoOverlay.
newtype GstPlaybin = GstPlaybin GI.Gst.Element

instance O.HasParentTypes GstPlaybin
type instance O.ParentTypes GstPlaybin = '[ GI.GstVideo.VideoOverlay ]

main :: IO ()
main = do

  -- Initialize GTK and Gstreamer
  _ <- GI.Gtk.init Nothing
  _ <- GI.Gst.init Nothing

  -- Create a new top level window
  window <- GI.Gtk.windowNew GI.Gtk.WindowTypeToplevel

  -- Create a drawing area for Gstreamer to render the video to
  -- Add this drawing area to the window we created
  -- Note that we size it to fit the video used in this example
  drawingArea <- GI.Gtk.drawingAreaNew
  GI.Gtk.widgetSetSizeRequest drawingArea 320 180
  GI.Gtk.containerAdd window drawingArea

  -- Create a playbin element and call it MultimediaPlayer
  playbin <- fmap fromJust (GI.Gst.elementFactoryMake "playbin" (Just "MultimediaPlayer"))
  -- When our window is ready...
  _ <- GI.Gtk.onWidgetRealize window $ do
    -- Get the underlying GDK window
    gdkWindow <- fmap fromJust (GI.Gtk.widgetGetWindow window)
    -- Get the underlying X11 window
    x11Window <- fmap fromJust (GI.Gtk.castTo GI.GdkX11.X11Window gdkWindow)

    -- Get the window handle or ID
    xid <- GI.GdkX11.x11WindowGetXid x11Window
    -- Convert xid from CULong to CUIntPtr
    let xid' = fromIntegral xid :: CUIntPtr

    -- Tell Gstreamer where it can render the output of playbin
    GI.GstVideo.videoOverlaySetWindowHandle (GstPlaybin playbin) xid'

    -- Tell Gstreamer to begin playing the video
    _ <- GI.Gst.elementSetState playbin GI.Gst.StatePlaying

    return ()

  -- Tell playbin which video file it should play
  -- Note that it could be a local file://
  Data.GI.Base.Properties.setObjectPropertyString
      playbin
      "uri"
      (Just "http://download.blender.org/peach/bigbuckbunny_movies/BigBuckBunny_320x180.mp4")

  -- Quit the GTK main loop when the window is destroyed
  _ <- GI.Gtk.onWidgetDestroy window GI.Gtk.mainQuit
  -- Render our window
  GI.Gtk.widgetShowAll window

  -- Start the GTK main loop
  GI.Gtk.main
