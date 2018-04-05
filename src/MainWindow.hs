module MainWindow(CmdOptions(..), run) where

import Data.List(intersect)
import Data.Maybe(isJust, fromMaybe)
import System.Exit(exitSuccess)
import System.FilePath((</>))

import Control.Monad(liftM)
import Control.Monad.Trans(lift, liftIO)
import Control.Monad.Trans.Maybe(MaybeT, runMaybeT)

import qualified Data.Text as Text
import qualified Control.Concurrent.STM as STM
import qualified System.Directory as Directory
import qualified System.FilePath as Path 

import qualified Data.GI.Base as GI
import qualified GI.Gtk as GTK
import qualified GI.Gdk as GDK
import qualified GI.Pango as Pango
import qualified GI.PangoCairo as PangoCairo
import qualified GI.GLib as Glib
import qualified Graphics.Rendering.Cairo as Cairo
import qualified GI.Cairo  
import qualified Data.String.Unicode

import qualified LoadSave 
import Rectangle
import Labyrinth
import Grid
import UserTexts
import GIHelper (renderWithContext, dialogRun, dialogAddButton)

data CmdOptions = CmdOptions
  { cmdBoxSize :: Int ,
    cmdBorderSize :: Int }

data LabyrinthState = LabyrinthState 
  { stRedrawFn    :: Rectangle Int -> IO (),
    stSetCursorFn :: GDK.CursorType -> IO (),
    stBoxSize     :: Int,
    stBorderSize  :: Int,
    stLabyrinth   :: STM.TVar (Maybe Labyrinth),
    stLegendDim   :: (Int, Int),
    stWindow      :: GTK.Window,
    stLanguage    :: Language }

run :: CmdOptions -> IO ()
run option = do
  let boxSize    = cmdBoxSize option
      borderSize = cmdBorderSize option
  GTK.init Nothing 
  language <- GTK.getDefaultLanguage
  languageString <- Pango.languageToString language 
  let labyLanguage = getLanguage $ Text.unpack languageString 
  legendDimensions <- computeLegendDimensions labyLanguage
  window <- GTK.windowNew GTK.WindowTypeToplevel
  canvas <- GTK.drawingAreaNew
  GTK.widgetAddEvents canvas [GDK.EventMaskButtonPressMask, 
                              GDK.EventMaskPointerMotionMask]
  GTK.containerAdd window canvas
  GTK.windowFullscreen window
  labyrinth  <- STM.atomically $ STM.newTVar Nothing  
  let state = LabyrinthState {
    stRedrawFn = redraw canvas,
    stSetCursorFn = setCursor canvas,
    stBoxSize = cmdBoxSize option,
    stBorderSize = cmdBorderSize option,
    stLabyrinth = labyrinth,
    stLegendDim = legendDimensions,
    stWindow = window,
    stLanguage = labyLanguage
  }
  disableEventCompression window
  GTK.onWidgetDestroy window GTK.mainQuit
  GTK.onWidgetKeyPressEvent window (keyPressHandler state)
  GTK.onWidgetSizeAllocate canvas (sizeChangeHandler state) 
  GTK.onWidgetDraw canvas (renderWithContext $ drawCanvasHandler state)
  GTK.onWidgetButtonPressEvent canvas (buttonPressHandler state)
  GTK.onWidgetMotionNotifyEvent canvas (motionNotifyHandler state)
  GTK.widgetShowAll window
  GTK.main

disableEventCompression :: GTK.Window -> IO ()
disableEventCompression window =
  do maybeGdkWindow <- GTK.widgetGetWindow window  
     case maybeGdkWindow of
         Just gdkWindow -> GDK.windowSetEventCompression gdkWindow False 
         Nothing        -> return ()

redraw :: GTK.DrawingArea -> Rectangle Int -> IO()
redraw drawingArea rectangle = let (x,y,width,height) = rToTuple fromIntegral rectangle
                               in GTK.widgetQueueDrawArea drawingArea x y width height

setCursor :: GTK.DrawingArea -> GDK.CursorType -> IO () 
setCursor window cursorType = do maybeWindow <- GTK.widgetGetParentWindow window
                                 case maybeWindow of 
                                     Just gdkWindow -> do display <- GDK.windowGetDisplay gdkWindow
                                                          cursor <- GDK.cursorNewForDisplay display cursorType 
                                                          GDK.windowSetCursor gdkWindow (Just cursor)
                                     Nothing     -> return ()

keyPressHandler :: LabyrinthState -> GDK.EventKey -> IO Bool
keyPressHandler state keyPressInfo = 
  do
    keyVal <- GDK.getEventKeyKeyval keyPressInfo
    keyName <- fromMaybe Text.empty <$> GDK.keyvalName keyVal
    case Text.unpack keyName of
      "Escape" -> saveAndQuit state                >> return True
      "s" -> setNextAction state SetStartField     >> return True
      "t" -> setNextAction state SetTargetField    >> return True
      "c" -> clearLabyrinth state                  >> return True
      "w" -> findPath state                        >> return True
      "r" -> resetPath state                       >> return True
      "F1" -> openSaveFileDialog state             >> return True
      "F2" -> openOpenFileDialog state             >> return True
      _    -> return False

sizeChangeHandler :: LabyrinthState -> GDK.Rectangle -> IO ()
sizeChangeHandler state rectangle =
  do let labyrinth = stLabyrinth state
     width <- GDK.getRectangleWidth rectangle 
     height <- GDK.getRectangleHeight rectangle
     let region = (width, height)
     STM.atomically $ do
       old <- STM.readTVar labyrinth
       let boxSize = stBoxSize state
           borderSize = stBorderSize state
           legendDimensions = stLegendDim state
       new <- labyConstruct old boxSize borderSize legendDimensions region 
       STM.writeTVar labyrinth (Just new)
     stSetCursorFn state GDK.CursorTypeTopLeftArrow

computeLegendDimensions :: Language -> IO (Int, Int)
computeLegendDimensions language =
    Cairo.withImageSurface Cairo.FormatRGB24 1920 1080 withSurface
  where withSurface :: Cairo.Surface -> IO (Int, Int)
        withSurface surface = Cairo.renderWith surface doRender
        doRender :: Cairo.Render (Int, Int)
        doRender = do setLegendTextStyle
                      extents <- Cairo.textExtents (renderLegend language)
                      return (ceiling $ Cairo.textExtentsWidth extents, 
                              ceiling $ Cairo.textExtentsHeight extents)

setLegendTextStyle :: Cairo.Render()
setLegendTextStyle = do Cairo.setAntialias Cairo.AntialiasSubpixel
                        Cairo.setSourceRGB 0 0 0                        
                        Cairo.setFontSize 13.0

drawCanvasHandler :: LabyrinthState -> GI.Cairo.Context -> Cairo.Render Bool
drawCanvasHandler state cairoContext = 
  do 
    let labyrinth = stLabyrinth state
    extents <- Cairo.clipExtents
    let drawRectangle = rFromBoundingBox round extents
    redrawInfo <- liftIO (getRedrawInfo labyrinth drawRectangle)
    drawLabyrinth cairoContext state drawRectangle redrawInfo
    return True
  where getRedrawInfo :: STM.TVar (Maybe Labyrinth) -> Rectangle Int -> IO (Maybe RedrawInfo)
        getRedrawInfo labyrinth drawRectangle = STM.atomically $
          do
            labyrinth <- STM.readTVar labyrinth
            labyGetRedrawInfo labyrinth drawRectangle

drawLabyrinth :: GI.Cairo.Context -> LabyrinthState -> Rectangle Int -> Maybe RedrawInfo -> Cairo.Render ()
drawLabyrinth _            _     _             Nothing     = return ()
drawLabyrinth cairoContext state drawRectangle (Just info) = 
  do
    clearArea 
    drawAxes (labyRedrIntersect info) (labyRedrGrid info)
    drawBoxes cairoContext state (labyRedrBoxes info)
    drawLegend (stLanguage state) (labyRedrLegend info) (grLegendRectangle $ labyRedrGrid info)

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

drawBoxes :: GI.Cairo.Context -> LabyrinthState 
                              -> [ (BoxState, RectangleInScreenCoordinates Int) ] 
                              -> Cairo.Render ()
drawBoxes cairoContext state = mapM_ drawBox
  where drawBox :: (BoxState, RectangleInScreenCoordinates Int) -> Cairo.Render ()
        drawBox (boxState, rectangle) = let (r,g,b) = labyStateToColor boxState
                                            (x,y,width,height) = rToTuple fromIntegral rectangle
                                        in do Cairo.save
                                              Cairo.setSourceRGB r g b
                                              Cairo.rectangle x y width height                                              
                                              Cairo.fill
                                              createBoxText cairoContext state boxState (x,y,width,height)
                                              Cairo.restore

createBoxText :: GI.Cairo.Context -> LabyrinthState -> BoxState 
                                  -> (Double, Double, Double, Double) 
                                  -> Cairo.Render ()
createBoxText cairoContext state boxState (x, y, width, height)
  | boxState `elem` [Empty, Border]       = return ()
  | boxState == StartField                = createBoxTextDo (translate (stLanguage state) BoxStartFieldText)
  | boxState == TargetField               = createBoxTextDo (translate (stLanguage state) BoxTargetFieldText)
  | Path direction <- boxState            = createBoxTextDo (computeBoxTextFromDirection direction)
  where
    computeBoxTextFromDirection direction = Data.String.Unicode.unicodeToXmlEntity $ case direction of 
                                                                                        North -> "⇑"  
                                                                                        NorthEast -> "⇗"
                                                                                        East -> "⇒" 
                                                                                        SouthEast -> "⇘"
                                                                                        South -> "⇓"
                                                                                        SouthWest -> "⇙"
                                                                                        West ->  "⇐"
                                                                                        NorthWest -> "⇖"
    pixelToPoint :: Double -> PangoCairo.FontMap -> IO Double
    pixelToPoint px fontMap = do resolution <- PangoCairo.fontMapGetResolution fontMap
                                 return $ px * 72 / resolution
    getMarkUp :: String -> Double -> String                                 
    getMarkUp text pt = "<span font=\"" ++ show (round pt) ++ "\">" ++ text ++ "</span>"
    createBoxTextDo :: String -> Cairo.Render ()
    createBoxTextDo boxText =
      do fontMap <- PangoCairo.fontMapGetDefault
         context <- liftIO Pango.contextNew
         Pango.contextSetFontMap context fontMap
         layout <- liftIO $ Pango.layoutNew context
         fontSize <- liftIO $ pixelToPoint width fontMap
         let markup = getMarkUp boxText fontSize 
         liftIO $ Pango.layoutSetMarkup layout (Text.pack markup) 
                                               (fromIntegral $ length markup)
         (_, extents) <- liftIO $ Pango.layoutGetPixelExtents layout
         rw <- fromIntegral <$> Pango.getRectangleWidth extents
         rh <- fromIntegral <$> Pango.getRectangleHeight extents 
         let lx = x + (width - fromIntegral rw) / 2
             ly = y + (height - fromIntegral rh) / 2
         Cairo.moveTo lx ly
         Cairo.setSourceRGB 0 0 0
         PangoCairo.showLayout cairoContext layout
                                            
drawLegend :: Language -> Bool -> Rectangle Int -> Cairo.Render ()
drawLegend language False _               = return ()
drawLegend language True  legendRectangle = 
  do 
    let (x,y) = rTopLeft legendRectangle
    Cairo.save
    Cairo.moveTo (fromIntegral x) (fromIntegral y)
    setLegendTextStyle
    Cairo.showText (renderLegend language)
    Cairo.restore

buttonPressHandler :: LabyrinthState -> GDK.EventButton -> IO Bool
buttonPressHandler state button = 
  do
    btnNo   <- GDK.getEventButtonButton button
    buttonX <- GDK.getEventButtonX button  
    buttonY <- GDK.getEventButtonY button
    let coordinates = (buttonX, buttonY)
    let markBox action = do handleMarkBox state coordinates action
                            return True 
    case btnNo of
      1  -> markBox SetAction    -- left mouse button
      3  -> markBox UnSetAction  -- right mouse button
      _  -> return False

motionNotifyHandler :: LabyrinthState -> GDK.EventMotion ->IO Bool
motionNotifyHandler state motion = 
  do
    motionX <- GDK.getEventMotionX motion  
    motionY <- GDK.getEventMotionY motion 
    let coordinates = (motionX, motionY)
    modifier <- GDK.getEventMotionState motion
    let mouseModifiers = intersect modifier [GDK.ModifierTypeButton1Mask, 
                                             GDK.ModifierTypeButton3Mask]
    let markBox action = do handleMarkBox state coordinates action
                            return True
    case mouseModifiers of
      [GDK.ModifierTypeButton1Mask] -> markBox SetAction
      [GDK.ModifierTypeButton3Mask] -> markBox UnSetAction
      _                             -> return False

handleMarkBox :: LabyrinthState -> PointInScreenCoordinates Double -> ActionType -> IO ()
handleMarkBox state (x, y) action = do let redraw = stRedrawFn state 
                                       redrawAreas <- handleMarkBoxDo
                                       case redrawAreas of 
                                           Just (areas, resetCursor) -> do mapM_ redraw areas
                                                                           doResetCursor resetCursor
                                           Nothing                   -> return ()
  where point = (round x, round y)
        handleMarkBoxDo = STM.atomically $ runMaybeT $ 
          do
            let labyrinth = stLabyrinth state
            old <- lift $ STM.readTVar labyrinth
            (new, redrawArea, resetCursor) <- labyMarkBox point action old
            lift $ STM.writeTVar labyrinth (Just new)
            return (redrawArea, resetCursor)
        doResetCursor False = return ()
        doResetCursor True = stSetCursorFn state GDK.CursorTypeTopLeftArrow 

setNextAction :: LabyrinthState -> NextAction -> IO ()
setNextAction state action = 
  do STM.atomically $ STM.readTVar (stLabyrinth state) >>= labySetNextAction action 
     stSetCursorFn state GDK.CursorTypeCrosshair 

clearLabyrinth :: LabyrinthState ->  IO ()
clearLabyrinth state = 
  do
    labyrinth <- STM.atomically $ 
      do old <- STM.readTVar (stLabyrinth state) 
         new <- labyClear old
         STM.writeTVar (stLabyrinth state) new 
         return new
    case labyrinth of 
      Just l -> let grid = labyGrid l 
                    (w,h) = grScreenSize grid
                in  stRedrawFn state (Rectangle 0 0 w h)   
      Nothing -> return ()

findPath :: LabyrinthState ->  IO ()
findPath state = 
  do
    result <- STM.atomically $ STM.readTVar (stLabyrinth state) >>= labyFindAndMark
    case result of 
        Left errorMsg -> errorPopup (stWindow state) (translate (stLanguage state) errorMsg)  
        Right path -> mapM_ (stRedrawFn state) path

resetPath :: LabyrinthState ->  IO ()
resetPath state = 
  do
    path <- STM.atomically $ STM.readTVar (stLabyrinth state) >>= labyResetPath
    mapM_ (stRedrawFn state) path 

saveAndQuit :: LabyrinthState -> IO ()
saveAndQuit state = 
  do file <- getSavedGameFile True 
     saveLabyrinth (stWindow state) state file
     GTK.mainQuit

saveLabyrinth :: GTK.IsWindow a => a -> LabyrinthState -> FilePath -> IO Bool
saveLabyrinth window state file =
  do let labyrinth = stLabyrinth state
     frozen <- STM.atomically $ STM.readTVar labyrinth >>= labyFreeze 
     saveResult <- LoadSave.saveLabyrinth frozen file 
     case saveResult of 
         Left msg -> do errorPopup window (translate (stLanguage state) msg) 
                        return False
         Right _  -> return True

loadLabyrinth :: GTK.IsWindow a => a -> LabyrinthState -> FilePath -> IO (Maybe Labyrinth) 
loadLabyrinth window state file = 
  do labyrinth <- LoadSave.loadLabyrinth file 
     case labyrinth of 
         Left errorMsg -> do let language = stLanguage state 
                                 translation = translate language errorMsg
                             errorPopup window translation
                             return Nothing
         Right labyrinth -> do unfrozen <- STM.atomically $ labyThaw labyrinth
                               return $ Just unfrozen
     
errorPopup :: GTK.IsWindow a => a -> String -> IO ()
errorPopup window text = 
  do dialog <- GI.new' GTK.MessageDialog [GTK.constructMessageDialogButtons GTK.ButtonsTypeOk]
     GTK.setMessageDialogMessageType dialog GTK.MessageTypeError
     GTK.setMessageDialogText dialog (Text.pack text)
     GTK.windowSetTransientFor dialog (Just window)
     GTK.setWindowWindowPosition dialog GTK.WindowPositionCenterOnParent
     dialogRun dialog
     GTK.widgetDestroy dialog

getSavedGameFile :: Bool -> IO FilePath
getSavedGameFile createDirectory
  | createDirectory = do directory <- getDirectory
                         Directory.createDirectoryIfMissing False directory
                         return (getFile directory)
  | otherwise       = getFile <$> getDirectory
  where getDirectory = Directory.getXdgDirectory Directory.XdgData "hlabyrinth" 
        getFile directory = directory </> ("last" ++ labyFileExtension)

openSaveFileDialog :: LabyrinthState -> IO ()     
openSaveFileDialog state =
  let caption = translate (stLanguage state) FileSaveCaption 
  in openFileDialog (stWindow state) GTK.FileChooserActionSave (stLanguage state) 
                    caption "gtk-save" onSuccess 
  where onSuccess window fileName = saveLabyrinth window state (addLabyFileExtension fileName)

openOpenFileDialog :: LabyrinthState -> IO ()     
openOpenFileDialog state =
  let caption = translate (stLanguage state) FileOpenCaption
  in openFileDialog (stWindow state) GTK.FileChooserActionOpen (stLanguage state) 
                    caption "gtk-open" onSuccess
  where onSuccess window fileName = 
          do let fileExt = addLabyFileExtension fileName 
             loadedLabyrinth <- loadLabyrinth window state fileExt
             newLabyrinth <- STM.atomically $
               do old <- STM.readTVar (stLabyrinth state) 
                  transformToNew old loadedLabyrinth
             repaintLabyrinth newLabyrinth 
        transformToNew :: Maybe Labyrinth -> Maybe Labyrinth -> STM.STM (Maybe Labyrinth)
        transformToNew Nothing _       = return Nothing
        transformToNew _       Nothing = return Nothing
        transformToNew (Just old) (Just new) = do let boxSize = stBoxSize state 
                                                      borderSize = stBorderSize state
                                                      legendDimensions = stLegendDim state
                                                      screenDimensions = grScreenSize $ labyGrid old
                                                  new <- labyConstruct (Just new) boxSize borderSize  
                                                                       legendDimensions screenDimensions
                                                  STM.writeTVar (stLabyrinth state) (Just new) 
                                                  return $ Just new
        repaintLabyrinth Nothing  = return False
        repaintLabyrinth (Just labyrinth) = do let grid  = labyGrid labyrinth 
                                                   (w,h) = grScreenSize grid
                                               stRedrawFn state (Rectangle 0 0 w h)     
                                               return True
        
openFileDialog :: GTK.Window  -> GTK.FileChooserAction
                              -> Language
                              -> String
                              -> String 
                              -> ( GTK.FileChooserDialog -> FilePath -> IO Bool) 
                              -> IO ()
openFileDialog window action language label button onSuccess =
  do dialog <- GI.new' GTK.FileChooserDialog [] 
     GTK.setWindowTitle dialog (Text.pack label)
     GTK.windowSetTransientFor dialog $ Just window
     GTK.fileChooserSetAction dialog action
     dialogAddButton dialog "gtk-cancel" GTK.ResponseTypeCancel
     dialogAddButton dialog button GTK.ResponseTypeAccept
     filter <- labyrinthFileFilter language
     GTK.fileChooserAddFilter dialog filter
     GTK.widgetShow dialog
     runDialog dialog
     GTK.widgetDestroy dialog 
  where runDialog dialog = do response <- dialogRun dialog 
                              rerun <- handleResponse dialog response
                              if rerun then runDialog dialog
                              else return ()
        handleResponse dialog GTK.ResponseTypeAccept = 
          do Just fileName <- GTK.fileChooserGetFilename dialog
             success <- onSuccess dialog fileName
             return $ not success
        handleResponse _ _ = return False

labyFileExtension :: String
labyFileExtension = ".laby"

labyFileExtensionWildcard :: String
labyFileExtensionWildcard = "*" ++ labyFileExtension

labyFileExtensionHelp :: Language -> String
labyFileExtensionHelp language = 
  translate language (LabyrinthFileFilter labyFileExtensionWildcard)
     
labyrinthFileFilter :: Language -> IO GTK.FileFilter
labyrinthFileFilter language =
  do labyFiles <- GTK.fileFilterNew
     GTK.fileFilterSetName labyFiles (Just $ Text.pack $ labyFileExtensionHelp language)
     GTK.fileFilterAddPattern labyFiles (Text.pack labyFileExtensionWildcard)
     return labyFiles

addLabyFileExtension :: FilePath -> FilePath 
addLabyFileExtension path =
  let (base, ext) = Path.splitExtension path
  in base ++ ext 
          ++ if ext == labyFileExtension then "" 
             else labyFileExtension
