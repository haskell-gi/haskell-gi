module MainWindow(CmdOptions(..), run) where

import Control.Monad.Trans(liftIO)
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
  GTK.widgetAddEvents canvas [GTK.ButtonPressMask, 
                              GTK.PointerMotionMask, 
                              GTK.PointerMotionHintMask]
  GTK.containerAdd window canvas
  GTK.windowFullscreen window
  GTK.on window GTK.objectDestroy     GTK.mainQuit
  GTK.on window GTK.keyPressEvent     (keyPressHandler state)
  GTK.on canvas GTK.configureEvent (sizeChangeHandler boxSize borderSize state)
  GTK.on canvas GTK.draw              (drawCanvasHandler state)
  GTK.on canvas GTK.buttonPressEvent  (buttonPressHandler state canvas)
  GTK.on canvas GTK.motionNotifyEvent (motionNotifyHandler state canvas)
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
drawCanvasHandler state = 
  do
    extents <- Cairo.clipExtents
    let drawRectangle = rFromBoundingBox round extents
    redrawInfo <- liftIO $ getRedrawInfo state drawRectangle
    case redrawInfo of
      Just info -> drawLabyrinth info
      _         -> return ()
  where getRedrawInfo :: STM.TVar (Maybe Labyrinth) -> Rectangle Int -> IO (Maybe RedrawInfo)
        getRedrawInfo state drawRectangle = STM.atomically $ 
          do
            labyrinth <- STM.readTVar state
            labyGetRedrawInfo labyrinth drawRectangle

drawLabyrinth :: RedrawInfo -> Cairo.Render ()
drawLabyrinth info = do
  Cairo.setAntialias Cairo.AntialiasSubpixel
  drawAxes (labyRedrIntersect info) (labyRedrGrid info)
  drawBoxes $ labyRedrBoxes info

drawAxes :: Rectangle Int -> Grid Int -> Cairo.Render ()
drawAxes area grid = do
  Cairo.save
  Cairo.setSourceRGB 0 0 0
  mapM_ drawLine (grAxesList area grid)
  Cairo.stroke
  Cairo.restore

drawLine :: Rectangle Int -> Cairo.Render ()
drawLine rectangle = do
  let (x, y, width, height) = rToTuple fromIntegral rectangle
  Cairo.rectangle x y width height
  Cairo.fill

drawBoxes :: [ (BoxState, RectangleInScreenCoordinates Int) ] -> Cairo.Render ()
drawBoxes = mapM_ drawBox
  where drawBox :: (BoxState, RectangleInScreenCoordinates Int) -> Cairo.Render ()
        drawBox (boxState, rectangle) = let (r,g,b) = labyStateToColor boxState
                                            (x,y,width,height) = rToTuple fromIntegral rectangle
                                        in do Cairo.setSourceRGB r g b
                                              Cairo.rectangle x y width height
                                              Cairo.fill

buttonPressHandler :: STM.TVar (Maybe Labyrinth) -> GTK.DrawingArea -> GTK.EventM GTK.EButton Bool
buttonPressHandler state drawingArea = GTK.tryEvent $ do
  button      <- GTK.eventButton
  coordinates <- GTK.eventCoordinates
  case button of
    GTK.LeftButton  -> liftIO $ handleMarkBox drawingArea state coordinates Border
    GTK.RightButton -> liftIO $ handleMarkBox drawingArea state coordinates Empty
  return ()

motionNotifyHandler :: STM.TVar (Maybe Labyrinth) -> GTK.DrawingArea -> GTK.EventM GTK.EMotion Bool
motionNotifyHandler state drawingArea = GTK.tryEvent $ 
  do
    coordinates <- GTK.eventCoordinates
    modifier    <- GTK.eventModifierMouse
    let mouseModifiers = intersect modifier [GTK.Button1, GTK.Button3]
    case mouseModifiers of
      [GTK.Button1] -> liftIO $ handleMarkBox drawingArea state coordinates Border
      [GTK.Button3] -> liftIO $ handleMarkBox drawingArea state coordinates Empty
    GTK.eventRequestMotions
    return ()

handleMarkBox
  :: GTK.DrawingArea -> STM.TVar (Maybe Labyrinth) -> PointInScreenCoordinates Double -> BoxState -> IO ()
handleMarkBox drawingArea state (x, y) boxValue =
  let point = (round x, round y)
  in do area <- getRedrawArea state point boxValue 
        case area of 
          Just a -> let (x,y,width,height) = rToTuple id a
                    in GTK.widgetQueueDrawArea drawingArea x y width height
          Nothing -> return ()
          
getRedrawArea :: STM.TVar (Maybe Labyrinth) -> PointInScreenCoordinates Int -> BoxState 
                                            -> IO (Maybe (RectangleInScreenCoordinates Int))
getRedrawArea state point boxValue = STM.atomically $
  do
    old <- STM.readTVar state
    markBox <- labyMarkBox point boxValue old
    case markBox of 
      Just (new, redrawArea) -> do STM.writeTVar state (Just new)
                                   return $ Just redrawArea
      Nothing -> return Nothing
        
        

