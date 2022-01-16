{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecursiveDo #-}

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)
import System.Environment (getArgs, getProgName)

import qualified GI.Adw as Adw
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk

import Data.GI.Base

sendHello :: Gtk.Stack -> Gtk.Spinner -> IO ()
sendHello stack spinner = void $ forkIO $ do
  -- Work hard for a while...
  threadDelay 2000000
  -- Show the result. idleAdd ensures that the GUI operations are run
  -- in the main thread.
  void $ GLib.idleAdd 0 $ do
    stack.setTransitionType Gtk.StackTransitionTypeCrossfade
    stack.setVisibleChildName "sent"
    spinner.stop
    return False

actionStack :: IO Gtk.Stack
actionStack = do
  stack <- new Gtk.Stack [#transitionType := Gtk.StackTransitionTypeUnderUp]

  rec
    button <- new Gtk.Button [
      #child :=> new Adw.ButtonContent [
          #iconName := "mail-send-symbolic",
          #label := "_Send hello",
          #useUnderline := True ],
      On #clicked $ do
          spinner.start
          stack.setVisibleChildName "spinner"
          sendHello stack spinner
      ]
    stack.addNamed button (Just "send")

    sending <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    spinner <- new Gtk.Spinner [#marginStart := 10]
    sending.append spinner
    label <- new Gtk.Label [#label := "Sending hello to the world...",
                            #hexpand := True]
    sending.append label
    stack.addNamed sending (Just "spinner")

    sent <- new Gtk.Label [#label := "Sent 👍"]
    stack.addNamed sent (Just "sent")

  return stack

activate :: Adw.Application -> IO ()
activate app = do
  content <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  title <- new Adw.WindowTitle [#title := "Hello World",
                                #subtitle := "from Haskell"]
  titlebar <- new Adw.HeaderBar [#titleWidget := title]
  content.append titlebar

  stack <- actionStack
  content.append stack

  window <- new Adw.ApplicationWindow [#application := app,
                                       #content := content,
                                       #defaultWidth := 400]
  window.present

main :: IO ()
main = do
  app <- new Adw.Application [#applicationId := "haskell-gi.Adw.test",
                              On #activate (activate ?self)]

  args <- getArgs
  progName <- getProgName
  void (app.run $ Just $ progName : args)
