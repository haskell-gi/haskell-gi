module MainWindow(CmdOptions(..), run) where

import Data.List(intersect)
import Data.Maybe(isJust)
import System.Exit(exitSuccess)
import System.FilePath((</>))

import Control.Monad.Trans(lift, liftIO)
import Control.Monad.Trans.Maybe(MaybeT, runMaybeT)

import qualified Data.Text as Text
import qualified Graphics.UI.Gtk as GTK
import qualified Graphics.Rendering.Cairo as Cairo
import qualified System.Glib as Glib
import qualified Graphics.Rendering.Pango.Layout as Pango
import qualified Control.Concurrent.STM as STM
import qualified System.Directory as Directory
import qualified System.FilePath as Path 

import qualified LoadSave 
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
  savedState <- getSavedGameFile False
  GTK.widgetAddEvents canvas [GTK.ButtonPressMask, 
                              GTK.PointerMotionMask, 
                              GTK.PointerMotionHintMask]
  GTK.containerAdd window canvas
  GTK.windowFullscreen window
  let redrawFn = redraw canvas
      setCursorFn = setCursor canvas
  GTK.on window GTK.objectDestroy     GTK.mainQuit
  GTK.on window GTK.keyPressEvent     (keyPressHandler state setCursorFn redrawFn window)
  GTK.on canvas GTK.configureEvent    (sizeChangeHandler 
    boxSize borderSize legendDimensions state setCursorFn)
  GTK.on canvas GTK.draw              (drawCanvasHandler state)
  GTK.on canvas GTK.buttonPressEvent  (buttonPressHandler state redrawFn setCursorFn)
  GTK.on canvas GTK.motionNotifyEvent (motionNotifyHandler state redrawFn setCursorFn)
  GTK.widgetShowAll window
  GTK.mainGUI

redraw :: GTK.DrawingArea -> Rectangle Int -> IO()
redraw drawingArea rectangle = let (x,y,width,height) = rToTuple id rectangle
                               in GTK.widgetQueueDrawArea drawingArea x y width height

setCursor :: GTK.DrawingArea -> GTK.CursorType -> IO () 
setCursor window cursorType = do drawWindow <- GTK.widgetGetParentWindow window
                                 cursor <- GTK.cursorNew cursorType
                                 GTK.drawWindowSetCursor drawWindow (Just cursor)
                                 return ()

keyPressHandler :: STM.TVar (Maybe Labyrinth) -> (GTK.CursorType -> IO ()) 
                                              -> (Rectangle Int -> IO ())  
                                              -> GTK.Window
                                              -> GTK.EventM GTK.EKey Bool
keyPressHandler state changeCursor redraw window = GTK.tryEvent $ 
  do
    keyName <- GTK.eventKeyName
    liftIO $ case Text.unpack keyName of
      "Escape" -> saveAndQuit window state
      "s" -> setNextAction state changeCursor SetStartField
      "t" -> setNextAction state changeCursor SetTargetField
      "c" -> clearLabyrinth state redraw
      "w" -> findPath state redraw
      "r" -> resetPath state redraw
      "F1" -> openSaveFileDialog state window
      "F2" -> openOpenFileDialog state window redraw

sizeChangeHandler
  :: Int -> Int -> (Int, Int) -> STM.TVar (Maybe Labyrinth) -> (GTK.CursorType -> IO ()) -> GTK.EventM GTK.EConfigure Bool
sizeChangeHandler boxSize borderSize legendDimensions state setCursor = 
  do
    region@(width, height) <- GTK.eventSize
    liftIO $ STM.atomically $ do
      old <- STM.readTVar state
      new <- labyConstruct old boxSize borderSize legendDimensions region 
      STM.writeTVar state (Just new)
    liftIO $ setCursor GTK.TopLeftArrow
    return True

computeLegendDimensions :: IO (Int, Int)
computeLegendDimensions =
    Cairo.withImageSurface Cairo.FormatRGB24 1920 1080 withSurface
  where withSurface :: Cairo.Surface -> IO (Int, Int)
        withSurface surface = Cairo.renderWith surface doRender
        doRender :: Cairo.Render (Int, Int)
        doRender = do setLegendTextStyle
                      extents <- Cairo.textExtents legend
                      return (ceiling $ Cairo.textExtentsWidth extents, 
                              ceiling $ Cairo.textExtentsHeight extents)

legend :: String 
legend = "LEFT BTN: DRAW | RIGHT BTN: CLEAR | ESC: QUIT | s: PLACE START | t: PLACE TARGET | c: CLEAR LABYRINTH | \ 
         \ w: FIND PATH | r: RESET PATH | F1: SAVE | F2: LOAD"

setLegendTextStyle :: Cairo.Render()
setLegendTextStyle = do Cairo.setAntialias Cairo.AntialiasSubpixel
                        Cairo.setSourceRGB 0 0 0                        
                        Cairo.setFontSize 13.0

drawCanvasHandler :: STM.TVar (Maybe Labyrinth) -> Cairo.Render ()
drawCanvasHandler state = 
  do 
    extents <- Cairo.clipExtents
    let drawRectangle = rFromBoundingBox round extents
    redrawInfo <- liftIO (getRedrawInfo state drawRectangle)
    drawLabyrinth drawRectangle redrawInfo
  where getRedrawInfo :: STM.TVar (Maybe Labyrinth) -> Rectangle Int -> IO (Maybe RedrawInfo)
        getRedrawInfo state drawRectangle = STM.atomically $
          do
            labyrinth <- STM.readTVar state
            labyGetRedrawInfo labyrinth drawRectangle

drawLabyrinth :: Rectangle Int -> Maybe RedrawInfo -> Cairo.Render ()
drawLabyrinth drawRectangle Nothing     = return ()
drawLabyrinth drawRectangle (Just info) = 
  do
    clearArea 
    drawAxes (labyRedrIntersect info) (labyRedrGrid info)
    drawBoxes $ labyRedrBoxes info
    drawLegend (labyRedrLegend info) (grLegendRectangle $ labyRedrGrid info)

clearArea :: Cairo.Render ()
clearArea = do Cairo.save
               Cairo.setSourceRGB 0.8 0.8 0.8
               Cairo.paint
               Cairo.restore

drawAxes :: Maybe (Rectangle Int) -> Grid Int -> Cairo.Render ()
drawAxes Nothing grid     = return ()
drawAxes (Just area) grid = do
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
                                        in do Cairo.save
                                              Cairo.setSourceRGB r g b
                                              Cairo.rectangle x y width height                                              
                                              Cairo.fill
                                              createBoxText boxState (x,y,width,height)
                                              Cairo.restore

createBoxText :: BoxState -> (Double, Double, Double, Double) -> Cairo.Render ()
createBoxText boxState (x, y, width, height)
  | boxState `elem` [Empty, Border, Path] = return ()
  | boxState == StartField                = createBoxTextDo "S"
  | boxState == TargetField               = createBoxTextDo "T"
  where
    pixelToPoint :: Double -> GTK.FontMap -> IO Double
    pixelToPoint px fontMap = do resolution <- GTK.cairoFontMapGetResolution fontMap
                                 return $ px * 72 / resolution
    getMarkUp :: String -> Double -> String                                 
    getMarkUp text pt = "<span font=\"" ++ show (round pt) ++ "\">" ++ text ++ "</span>"
    setMarkUp :: Pango.PangoLayout -> String -> IO String 
    setMarkUp layout string = Glib.glibToString <$> Pango.layoutSetMarkup layout (Glib.stringToGlib string)
    createBoxTextDo :: String -> Cairo.Render ()
    createBoxTextDo text =
      do fontMap <- liftIO GTK.cairoFontMapGetDefault
         context <- liftIO $ GTK.cairoCreateContext (Just fontMap)
         layout <- liftIO $ GTK.layoutEmpty context
         fontSize <- liftIO $ pixelToPoint width fontMap
         let markup = getMarkUp text fontSize 
         liftIO $ setMarkUp layout markup
         (_, GTK.Rectangle _ _ lw lh) <- liftIO $ Pango.layoutGetPixelExtents layout
         let lx = x + (width - fromIntegral lw) / 2
             ly = y + (height - fromIntegral lh) / 2
         Cairo.moveTo lx ly
         GTK.updateLayout layout
         Cairo.setSourceRGB 0 0 0
         GTK.showLayout layout
                                            
drawLegend :: Bool -> Rectangle Int -> Cairo.Render ()
drawLegend False _               = return ()
drawLegend True  legendRectangle = 
  do 
    let (x,y) = rTopLeft legendRectangle
    Cairo.save
    Cairo.moveTo (fromIntegral x) (fromIntegral y)
    setLegendTextStyle
    Cairo.showText legend
    Cairo.restore

buttonPressHandler :: STM.TVar (Maybe Labyrinth) -> (Rectangle Int -> IO ()) 
                                                 -> (GTK.CursorType -> IO ())
                                                 -> GTK.EventM GTK.EButton Bool
buttonPressHandler state redraw changeCursor = GTK.tryEvent $ do
  button      <- GTK.eventButton
  coordinates <- GTK.eventCoordinates
  case button of
    GTK.LeftButton  -> liftIO $ handleMarkBox state redraw changeCursor coordinates SetAction
    GTK.RightButton -> liftIO $ handleMarkBox state redraw changeCursor coordinates UnSetAction
  return ()

motionNotifyHandler :: STM.TVar (Maybe Labyrinth) -> (Rectangle Int -> IO ()) 
                                                  -> (GTK.CursorType -> IO ())
                                                  -> GTK.EventM GTK.EMotion Bool
motionNotifyHandler state redraw changeCursor = GTK.tryEvent $ 
  do
    coordinates <- GTK.eventCoordinates
    modifier    <- GTK.eventModifierMouse
    let mouseModifiers = intersect modifier [GTK.Button1, GTK.Button3]
    case mouseModifiers of
      [GTK.Button1] -> liftIO $ handleMarkBox state redraw changeCursor coordinates SetAction
      [GTK.Button3] -> liftIO $ handleMarkBox state redraw changeCursor coordinates UnSetAction
    GTK.eventRequestMotions
    return ()

handleMarkBox :: STM.TVar (Maybe Labyrinth) -> (Rectangle Int -> IO ()) 
                                            -> (GTK.CursorType -> IO ())
                                            -> PointInScreenCoordinates Double 
                                            -> ActionType 
                                            -> IO ()
handleMarkBox state redraw changeCursor (x, y) action = do redrawAreas <- handleMarkBoxDo
                                                           case redrawAreas of 
                                                               Just (areas, resetCursor) -> do mapM_ redraw areas
                                                                                               doResetCursor resetCursor
                                                               Nothing                   -> return ()
  where point = (round x, round y)
        handleMarkBoxDo = STM.atomically $ runMaybeT $ 
          do
            old <- lift $ STM.readTVar state
            (new, redrawArea, resetCursor) <- labyMarkBox point action old
            lift $ STM.writeTVar state (Just new)
            return (redrawArea, resetCursor)
        doResetCursor False = return ()
        doResetCursor True = changeCursor GTK.TopLeftArrow

setNextAction :: STM.TVar (Maybe Labyrinth) -> (GTK.CursorType -> IO ()) -> NextAction -> IO ()
setNextAction state changeCursor action = 
  do STM.atomically $ STM.readTVar state >>= labySetNextAction action >>= STM.writeTVar state
     changeCursor GTK.Cross            

clearLabyrinth :: STM.TVar (Maybe Labyrinth) -> (Rectangle Int -> IO ()) ->  IO ()
clearLabyrinth state redraw = 
  do
    labyrinth <- STM.atomically $ 
      do old <- STM.readTVar state 
         new <- labyClear old
         STM.writeTVar state new 
         return new
    case labyrinth of 
      Just l -> let grid = labyGrid l 
                    (w,h) = grScreenSize grid
                in  redraw (Rectangle 0 0 w h)   
      Nothing -> return ()

findPath :: STM.TVar (Maybe Labyrinth) -> (Rectangle Int -> IO ()) ->  IO ()
findPath state redraw = 
  do
    path <- STM.atomically $ STM.readTVar state >>= labyFindAndMark
    mapM_ redraw path

resetPath :: STM.TVar (Maybe Labyrinth) -> (Rectangle Int -> IO ()) ->  IO ()
resetPath state redraw = 
  do
    path <- STM.atomically $ STM.readTVar state >>= labyResetPath
    mapM_ redraw path 

saveAndQuit :: GTK.WindowClass a => a -> STM.TVar (Maybe Labyrinth) -> IO ()
saveAndQuit window state = 
  do file <- getSavedGameFile True 
     saveLabyrinth window state file
     GTK.mainQuit

saveLabyrinth :: GTK.WindowClass a => a -> STM.TVar (Maybe Labyrinth) -> FilePath -> IO ()
saveLabyrinth window state file =
  do labyrinth <- STM.atomically $ STM.readTVar state >>= labyFreeze 
     LoadSave.saveLabyrinth labyrinth file (errorPopup window) 

loadLabyrinth :: GTK.WindowClass a => a -> FilePath -> IO (Maybe Labyrinth) 
loadLabyrinth window file = 
  do labyrinth <- LoadSave.loadLabyrinth file (errorPopup window)
     case labyrinth of 
         Just laby -> do unfrozen <- STM.atomically $ labyThaw laby
                         return $ Just unfrozen
         Nothing -> return Nothing
     
errorPopup :: GTK.WindowClass a => a -> String -> IO ()
errorPopup window text = 
  do msg <- GTK.messageDialogNew (Just (GTK.toWindow window))
               [GTK.DialogModal, GTK.DialogDestroyWithParent]
               GTK.MessageError GTK.ButtonsClose
               text
     GTK.dialogRun msg
     GTK.widgetDestroy msg

getSavedGameFile :: Bool -> IO FilePath
getSavedGameFile createDirectory
  | createDirectory = do directory <- getDirectory
                         Directory.createDirectoryIfMissing False directory
                         return (getFile directory)
  | otherwise       = getFile <$> getDirectory
  where getDirectory = Directory.getXdgDirectory Directory.XdgData "hlabyrinth" 
        getFile directory = directory </> ("last" ++ labyFileExtension)

openSaveFileDialog :: STM.TVar (Maybe Labyrinth) -> GTK.Window -> IO ()     
openSaveFileDialog state window =
  openFileDialog state window GTK.FileChooserActionSave "Save labyrinth" 
                 "gtk-save" onSuccess 
  where onSuccess window fileName = saveLabyrinth window state (addLabyFileExtension fileName)

openOpenFileDialog :: STM.TVar (Maybe Labyrinth) -> GTK.Window 
                                                 -> (Rectangle Int -> IO ())   
                                                 -> IO ()     
openOpenFileDialog state window redraw =
  openFileDialog state window GTK.FileChooserActionOpen "Open labyrinth" 
                 "gtk-open" onSuccess
  where onSuccess window fileName = 
          do let fileExt = addLabyFileExtension fileName 
             loadedLabyrinth <- loadLabyrinth window fileExt
             case loadedLabyrinth of 
                 Just labyrinth -> do STM.atomically $ STM.writeTVar state (Just labyrinth)
                                      let grid  = labyGrid labyrinth 
                                          (w,h) = grScreenSize grid
                                      redraw (Rectangle 0 0 w h)    
                 Nothing -> return () 

openFileDialog :: STM.TVar (Maybe Labyrinth) -> GTK.Window  
                                             -> GTK.FileChooserAction
                                             -> String
                                             -> String 
                                             -> ( GTK.FileChooserDialog -> FilePath -> IO ()) 
                                             -> IO ()
openFileDialog state window action label button onSuccess =
  do dialog <- GTK.fileChooserDialogNew
                (Just label)
                (Just window)
                action
                [("gtk-cancel", GTK.ResponseCancel),
                (button, GTK.ResponseAccept)]
     filter <- labyrinthFileFilter
     GTK.fileChooserAddFilter dialog filter
     GTK.widgetShow dialog
     response <- GTK.dialogRun dialog
     case response of 
         GTK.ResponseAccept -> do Just fileName <- GTK.fileChooserGetFilename dialog
                                  onSuccess dialog fileName
         _                  -> return ()
     GTK.widgetHide dialog  

labyFileExtension :: String
labyFileExtension = ".laby"

labyFileExtensionWildcard :: String
labyFileExtensionWildcard = "*" ++ labyFileExtension

labyFileExtensionHelp :: String
labyFileExtensionHelp = 
  "Labyrinth files (" ++ labyFileExtensionWildcard ++ ")"
     
labyrinthFileFilter :: IO GTK.FileFilter
labyrinthFileFilter =
  do labyFiles <- GTK.fileFilterNew
     GTK.fileFilterSetName labyFiles labyFileExtensionHelp
     GTK.fileFilterAddPattern labyFiles labyFileExtensionWildcard
     return labyFiles

addLabyFileExtension :: FilePath -> FilePath 
addLabyFileExtension path =
  let (base, ext) = Path.splitExtension path
  in base ++ ext 
          ++ if ext == labyFileExtension then "" 
             else labyFileExtension
