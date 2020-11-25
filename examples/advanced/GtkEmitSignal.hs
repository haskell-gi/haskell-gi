{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

{-
  A minimal example of how to emit a signal to a GTK widget.

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

import Data.GI.Base.GValue
import Data.GI.Base.ManagedPtr
import Data.GI.Base
import GI.GObject.Functions
import qualified GI.Gtk

import Data.Text (Text)

main :: IO ()
main = do
  -- Initialize GTK
  _ <- GI.Gtk.init Nothing

  -- Create a new window with the title `GTK Emit Signal Example`
  win <- new GI.Gtk.Window [ #title := "GTK Emit Signal Example" ]

  -- When we destroy (close) the window, quit the GTK main loop
  _ <- on win #destroy GI.Gtk.mainQuit

  -- Create a new text entry widget
  -- After we emit our signal, the text inside the box will say `Haskell`
  entry <- new GI.Gtk.Entry [ #text := "askell" ]

  -- Add the text entry to our window
  #add win entry

  -- When the text entry is ready
  _ <- on entry #realize $ do
    -- Get the GType for the GtkEntry
    gtype <- glibType @GI.Gtk.Entry

    -- Get the signal ID and Quark detail
    -- based on the `insert-at-cursor` signal name
    -- and the GType GtkEntry
    (_, signalId, detail) <- signalParseName "insert-at-cursor" gtype False

    -- Create a managed GValue* with the type GObject
    object <- toGValue (Just entry)

    -- Create a managed GValue* with the type gchar* (string)
    -- The `H` is the character we will insert at the beginning
    -- of the text entry to complete the word Haskell
    string <- toGValue (Just "H" :: Maybe Text)

    -- Emit the signal to object making sure to place
    -- it first in the parameter array
    -- The rest of the array holds the parameters that the
    -- signal accepts
    -- In this case `insert-at-cursor` accepts a string
    -- parameter
    _ <- signalEmitv [object, string] signalId detail

    return ()

  -- Show the window containing our text entry
  #showAll win

  -- Run the GTK main loop
  GI.Gtk.main
