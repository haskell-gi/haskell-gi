{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (void)
import qualified Data.GI.Base as GI
import qualified Data.IORef as IORef
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified GI.AyatanaAppIndicator3 as AI
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import qualified System.Environment as Env

data State = State
  { stPercentage :: Int,
    stCanHazLabel :: Bool,
    stActive :: Bool
  }

main :: IO ()
main = do
  void . Gtk.init . Just . fmap T.pack =<< Env.getArgs

  indicator <-
    AI.indicatorNew
      "example-simple-client"
      "indicator-messages"
      AI.IndicatorCategoryApplicationStatus

  AI.indicatorSetStatus indicator AI.IndicatorStatusActive
  AI.indicatorSetAttentionIconFull
    indicator
    "indicator-messages-new"
    "System Messages Icon Highlighted"
  AI.indicatorSetLabel indicator "1%" "100%"
  AI.indicatorSetTitle indicator $ Just "Test Indicator"

  GI.on indicator #scrollEvent scrollEventCallback

  refState <-
    IORef.newIORef $
      State
        { stPercentage = 0,
          stCanHazLabel = True,
          stActive = True,
          ..
        }
  GLib.timeoutAddSeconds GLib.PRIORITY_DEFAULT 1 $
    percentChange indicator refState

  menu <- Gtk.menuNew

  item1 <- Gtk.checkMenuItemNewWithLabel "1"
  GI.on item1 #activate $ itemClickedCallback "1"
  Gtk.menuShellAppend menu item1
  Gtk.widgetShow item1

  item2 <- Gtk.radioMenuItemNewWithLabel ([] :: [Gtk.RadioMenuItem]) "2"
  GI.on item2 #activate $ itemClickedCallback "2"
  Gtk.menuShellAppend menu item2
  Gtk.widgetShow item2

  item3 <- Gtk.menuItemNewWithLabel "3"
  Gtk.menuShellAppend menu item3
  appendSubmenu item3
  Gtk.widgetShow item3

  toggleItem <- Gtk.menuItemNewWithLabel "Toggle 3"
  GI.on toggleItem #activate $ toggleSensitivityCallback item3
  Gtk.menuShellAppend menu toggleItem
  Gtk.widgetShow toggleItem

  image <-
    Gtk.imageNewFromIconName
      (Just "document-new")
      (fromIntegral $ fromEnum Gtk.IconSizeMenu)
  label <- Gtk.labelNew $ Just "New"
  box <- Gtk.boxNew Gtk.OrientationHorizontal 0
  Gtk.boxPackStart box image False False 0
  Gtk.boxPackStart box label False False 0
  imageItem <- Gtk.menuItemNew
  Gtk.containerAdd imageItem box
  Gtk.on imageItem #activate $ imageClickedCallback image
  Gtk.menuShellAppend menu imageItem
  Gtk.widgetShowAll imageItem

  attentionImage <- Gtk.menuItemNewWithLabel "Get Attention"
  Gtk.on attentionImage #activate $ activateClickedCallback indicator refState
  Gtk.menuShellAppend menu attentionImage
  Gtk.widgetShow attentionImage
  AI.indicatorSetSecondaryActivateTarget indicator $ Just attentionImage

  labelItem <- Gtk.menuItemNewWithLabel "Hide label"
  Gtk.on labelItem #activate $ labelToggleCallback refState labelItem
  Gtk.menuShellAppend menu labelItem
  Gtk.widgetShow labelItem

  iconItem <- Gtk.checkMenuItemNewWithLabel "Set Local Icon"
  Gtk.on iconItem #activate $ localIconToggle indicator iconItem
  Gtk.menuShellAppend menu iconItem
  Gtk.widgetShow iconItem

  AI.indicatorSetMenu indicator $ Just menu

  mainLoop <- GLib.mainLoopNew Nothing False
  GLib.mainLoopRun mainLoop

labelToggleCallback :: IORef.IORef State -> Gtk.MenuItem -> IO ()
labelToggleCallback refState menuItem = do
  IORef.modifyIORef' refState $ \st@State {..} ->
    st {stCanHazLabel = not stCanHazLabel}
  canHazLabel <- stCanHazLabel <$> IORef.readIORef refState
  if canHazLabel
    then Gtk.set menuItem [#label Gtk.:= "Hide label"]
    else Gtk.set menuItem [#label Gtk.:= "Show label"]

activateClickedCallback :: AI.Indicator -> IORef.IORef State -> IO ()
activateClickedCallback indicator refState = do
  active <- stActive <$> IORef.readIORef refState
  if active
    then do
      AI.indicatorSetStatus indicator AI.IndicatorStatusAttention
      AI.indicatorSetLabel indicator "I'm okay now" ""
      IORef.modifyIORef' refState $ \st -> st {stActive = False}
    else do
      AI.indicatorSetStatus indicator AI.IndicatorStatusActive
      AI.indicatorSetLabel indicator "Get Attention" ""
      IORef.modifyIORef' refState $ \st -> st {stActive = True}

localIconToggle :: AI.Indicator -> Gtk.CheckMenuItem -> IO ()
localIconToggle indicator item = do
  active <- Gtk.get item #active
  if active
    then
      AI.indicatorSetIconFull indicator "./simple-client-test-icon.png" "Local Icon"
    else
      AI.indicatorSetIconFull indicator "indicator-messages" "System Icon"

itemClickedCallback :: T.Text -> IO ()
itemClickedCallback label = T.putStrLn $ label <> " clicked!"

toggleSensitivityCallback :: Gtk.MenuItem -> IO ()
toggleSensitivityCallback menuItem = do
  sensitive <- Gtk.get menuItem #sensitive
  Gtk.set menuItem [#sensitive Gtk.:= not sensitive]

imageClickedCallback :: Gtk.Image -> IO ()
imageClickedCallback image = do
  Gtk.imageSetFromIconName
    image
    (Just "document-open")
    (fromIntegral $ fromEnum Gtk.IconSizeMenu)

scrollEventCallback :: AI.IndicatorScrollEventCallback
scrollEventCallback delta direction = do
  T.putStrLn $
    "Got scroll event! delta: "
      <> T.pack (show delta)
      <> ", direction: "
      <> T.pack (show direction)

appendSubmenu :: Gtk.MenuItem -> IO ()
appendSubmenu menuItem = do
  menu <- Gtk.menuNew

  itemSub1 <- Gtk.menuItemNewWithLabel "Sub 1"
  Gtk.menuShellAppend menu itemSub1
  GI.on itemSub1 #activate $ itemClickedCallback "Sub 1"

  itemSub2 <- Gtk.menuItemNewWithLabel "Sub 2"
  Gtk.menuShellAppend menu itemSub2
  GI.on itemSub2 #activate $ toggleSensitivityCallback itemSub1

  itemSub3 <- Gtk.menuItemNewWithLabel "Sub 3"
  Gtk.menuShellAppend menu itemSub3
  GI.on itemSub3 #activate $ itemClickedCallback "Sub 3"

  Gtk.widgetShowAll menu

  Gtk.menuItemSetSubmenu menuItem (Just menu)

percentChange :: AI.Indicator -> IORef.IORef State -> IO Bool
percentChange indicator refState = do
  IORef.modifyIORef' refState $ \st@State {..} ->
    st {stPercentage = (stPercentage + 1) `mod` 100}

  State {..} <- IORef.readIORef refState
  if stCanHazLabel
    then
      AI.indicatorSetLabel
        indicator
        (T.pack $ show (stPercentage + 1) <> "%")
        "100%"
    else AI.indicatorSetLabel indicator "" ""

  pure True
