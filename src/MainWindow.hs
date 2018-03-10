module MainWindow(CmdOptions(..), run) where

import Control.Monad.Trans(liftIO)
import Control.Arrow((&&&))
import System.Exit(exitSuccess)
import Data.List(intersect)

import qualified Data.Text as Text
import qualified Graphics.UI.Gtk as GTK
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Control.Concurrent.STM as STM

import Rectangle
import Labyrinth
import Grid

data CmdOptions = CmdOptions
  { cmdBoxSize :: Int ,
    cmdBorderSize :: Int }

run :: CmdOptions -> IO ()
run option = do
  let boxSize    = cmdBoxSize option
      borderSize = cmdBorderSize option
  GTK.initGUI
  window <- GTK.windowNew
  canvas <- GTK.drawingAreaNew
  state  <- STM.atomically $ STM.newTVar Nothing
  GTK.widgetAddEvents
    canvas
    [GTK.ButtonPressMask, GTK.PointerMotionMask, GTK.PointerMotionHintMask]
  GTK.containerAdd window canvas
  GTK.windowFullscreen window
  GTK.on window GTK.objectDestroy     GTK.mainQuit
  GTK.on window GTK.keyPressEvent     (keyPressHandler state)
  GTK.on canvas GTK.configureEvent (sizeChangeHandler boxSize borderSize state)
  GTK.on canvas GTK.draw              (drawCanvasHandler state)
  GTK.on canvas GTK.buttonPressEvent  (buttonPressHandler state)
  GTK.on canvas GTK.motionNotifyEvent (motionNotifyHandler state)
  GTK.widgetShowAll window
  GTK.mainGUI

keyPressHandler :: STM.TVar (Maybe Labyrinth) -> GTK.EventM GTK.EKey Bool
keyPressHandler _ = GTK.tryEvent $ do
  keyName <- GTK.eventKeyName
  liftIO $ case Text.unpack keyName of
    "Escape" -> GTK.mainQuit

sizeChangeHandler
  :: Int -> Int -> STM.TVar (Maybe Labyrinth) -> GTK.EventM GTK.EConfigure Bool
sizeChangeHandler boxSize borderSize state = do
  region <- GTK.eventSize
  liftIO $ STM.atomically $ do
    labyrinth <- labyConstruct boxSize borderSize region
    STM.writeTVar state (Just labyrinth)
  return True

drawCanvasHandler :: STM.TVar (Maybe Labyrinth) -> Cairo.Render ()
drawCanvasHandler state = do
  extents <- Cairo.clipExtents
  let drawRectangle = rFromBoundingBox round extents
  grid <- liftIO $ getLabyrinthState
    state
    (rIntersect drawRectangle . grRectangle . labyGrid)
  case grid of
    Just (Just intersection, grid) -> drawLabyrinth intersection grid
    _                              -> return ()

drawLabyrinth :: Rectangle Int -> Grid Int -> Cairo.Render ()
drawLabyrinth area grid = do
  Cairo.setAntialias Cairo.AntialiasSubpixel
  drawAxes area grid

drawAxes :: Rectangle Int -> Grid Int -> Cairo.Render ()
drawAxes area grid = do
  Cairo.save
  Cairo.setSourceRGB 0 0 0
  mapM_ drawLine (grAxesList area grid)
  Cairo.stroke
  Cairo.restore

drawLine :: Rectangle Int -> Cairo.Render ()
drawLine rectangle = do
  let (x1, y1, x2, y2) = rToTuple fromIntegral rectangle
  Cairo.rectangle x1 y1 x2 y2
  Cairo.fill

getLabyrinthState
  :: STM.TVar (Maybe Labyrinth) -> (Labyrinth -> a) -> IO (Maybe (a, Grid Int))
getLabyrinthState state f = STM.atomically $ do
  labyrinth <- STM.readTVar state
  return $ fmap (f &&& labyGrid) labyrinth

buttonPressHandler :: STM.TVar (Maybe Labyrinth) -> GTK.EventM GTK.EButton Bool
buttonPressHandler state = GTK.tryEvent $ do
  button      <- GTK.eventButton
  coordinates <- GTK.eventCoordinates
  case button of
    GTK.LeftButton  -> liftIO $ handleMarkBox state coordinates Border
    GTK.RightButton -> liftIO $ handleMarkBox state coordinates Empty
  return ()

motionNotifyHandler :: STM.TVar (Maybe Labyrinth) -> GTK.EventM GTK.EMotion Bool
motionNotifyHandler state = GTK.tryEvent $ do
  coordinates <- GTK.eventCoordinates
  modifier    <- GTK.eventModifierMouse
  let mouseModifiers = intersect modifier [GTK.Button1, GTK.Button2]
  case mouseModifiers of
    [GTK.Button1] -> liftIO $ handleMarkBox state coordinates Border
    [GTK.Button3] -> liftIO $ handleMarkBox state coordinates Empty
  GTK.eventRequestMotions
  return ()

handleMarkBox
  :: STM.TVar (Maybe Labyrinth) -> (Double, Double) -> BoxState -> IO ()
handleMarkBox state (x, y) boxValue =
  let point = (round x, round y)
  in  STM.atomically $ do
        labyrinth <- STM.readTVar state
        STM.writeTVar state =<< labyMarkBox point boxValue labyrinth

