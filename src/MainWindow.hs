module MainWindow(CmdOptions(..), run) where

import Control.Monad.Morph(hoist)
import Control.Monad.Trans(lift, liftIO)
import Control.Monad.Trans.Maybe(MaybeT, runMaybeT)
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
  legendDimensions <- computeLegendDimensions
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
  GTK.on canvas GTK.configureEvent    (sizeChangeHandler boxSize borderSize state)
  GTK.on canvas GTK.draw              (drawCanvasHandler state)
  let redrawFn = redraw canvas
  GTK.on canvas GTK.buttonPressEvent  (buttonPressHandler state redrawFn)
  GTK.on canvas GTK.motionNotifyEvent (motionNotifyHandler state redrawFn)
  GTK.widgetShowAll window
  GTK.mainGUI

redraw :: GTK.DrawingArea -> Maybe (Rectangle Int) -> IO()
redraw _           Nothing           = return ()
redraw drawingArea (Just rectangle)  = let (x,y,width,height) = rToTuple id rectangle
                                       in GTK.widgetQueueDrawArea drawingArea x y width height

keyPressHandler :: STM.TVar (Maybe Labyrinth) -> GTK.EventM GTK.EKey Bool
keyPressHandler _ = GTK.tryEvent $ 
  do
    keyName <- GTK.eventKeyName
    liftIO $ case Text.unpack keyName of
      "Escape" -> GTK.mainQuit

sizeChangeHandler
  :: Int -> Int -> STM.TVar (Maybe Labyrinth) -> GTK.EventM GTK.EConfigure Bool
sizeChangeHandler boxSize borderSize state = 
  do
    region@(width, height) <- GTK.eventSize
    liftIO $ STM.atomically $ do
      labyrinth <- labyConstruct boxSize borderSize region
      STM.writeTVar state (Just labyrinth)
    return True

computeLegendDimensions :: IO (Int, Int)
computeLegendDimensions =
    Cairo.withImageSurface Cairo.FormatRGB24 1920 1080 withSurface
  where withSurface :: Cairo.Surface -> IO (Int, Int)
        withSurface surface = Cairo.renderWith surface doRender
        doRender :: Cairo.Render (Int, Int)
        doRender = do Cairo.setAntialias Cairo.AntialiasSubpixel
                      Cairo.setFontSize 13.0
                      extents <- Cairo.textExtents legend
                      return (ceiling $ Cairo.textExtentsWidth extents, 
                              ceiling $ Cairo.textExtentsHeight extents)

legend :: String 
legend = "LEFT BTN: DRAW | RIGHT BTN: CLEAR | ESC: QUIT"

drawCanvasHandler :: STM.TVar (Maybe Labyrinth) -> Cairo.Render ()
drawCanvasHandler state = 
  do 
    runMaybeT $ 
      do
        extents <- lift Cairo.clipExtents
        let drawRectangle = rFromBoundingBox round extents
        redrawInfo <- hoist liftIO (getRedrawInfo state drawRectangle)
        lift $ drawLabyrinth redrawInfo
    return ()
  where getRedrawInfo :: STM.TVar (Maybe Labyrinth) -> Rectangle Int -> MaybeT IO RedrawInfo
        getRedrawInfo state drawRectangle = hoist STM.atomically $
          do
            labyrinth <- lift $ STM.readTVar state
            labyGetRedrawInfo labyrinth drawRectangle


drawLabyrinth :: RedrawInfo -> Cairo.Render ()
drawLabyrinth info = do
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

buttonPressHandler :: STM.TVar (Maybe Labyrinth) -> (Maybe (Rectangle Int) -> IO ()) 
                                                 -> GTK.EventM GTK.EButton Bool
buttonPressHandler state redraw = GTK.tryEvent $ do
  button      <- GTK.eventButton
  coordinates <- GTK.eventCoordinates
  case button of
    GTK.LeftButton  -> liftIO $ handleMarkBox state redraw coordinates Border
    GTK.RightButton -> liftIO $ handleMarkBox state redraw coordinates Empty
  return ()

motionNotifyHandler :: STM.TVar (Maybe Labyrinth) -> (Maybe (Rectangle Int) -> IO ()) 
                                                  -> GTK.EventM GTK.EMotion Bool
motionNotifyHandler state redraw = GTK.tryEvent $ 
  do
    coordinates <- GTK.eventCoordinates
    modifier    <- GTK.eventModifierMouse
    let mouseModifiers = intersect modifier [GTK.Button1, GTK.Button3]
    case mouseModifiers of
      [GTK.Button1] -> liftIO $ handleMarkBox state redraw coordinates Border
      [GTK.Button3] -> liftIO $ handleMarkBox state redraw coordinates Empty
    GTK.eventRequestMotions
    return ()

handleMarkBox :: STM.TVar (Maybe Labyrinth) -> (Maybe (Rectangle Int) -> IO ()) 
                                            -> PointInScreenCoordinates Double -> BoxState -> IO ()
handleMarkBox state redraw (x, y) boxValue = do redrawArea <- handleMarkBoxDo
                                                redraw redrawArea
  where point = (round x, round y)
        handleMarkBoxDo = STM.atomically $ runMaybeT $ 
          do
            old <- lift $ STM.readTVar state
            (new, redrawArea) <- labyMarkBox point boxValue old
            lift $ STM.writeTVar state (Just new)
            return redrawArea
            
