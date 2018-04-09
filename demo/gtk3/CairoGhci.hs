-- Example of an drawing graphics onto a canvas.
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Control.Monad.Trans ( liftIO )
import Graphics.UI.Gtk.Gdk.EventM

run :: Render () -> IO ()
run act = do
  initGUI
  dia <- dialogNew
  dialogAddButton dia stockClose ResponseClose
  contain <- dialogGetUpper dia
  canvas <- drawingAreaNew
  canvas `onSizeRequest` return (Requisition 250 250)
  canvas `on` exposeEvent $ tryEvent $ updateCanvas canvas act
  boxPackStartDefaults contain canvas
  widgetShow canvas
  dialogRun dia
  widgetDestroy dia
  -- Flush all commands that are waiting to be sent to the graphics server.
  -- This ensures that the window is actually closed before ghci displays the
  -- prompt again.
  flush

  where updateCanvas :: DrawingArea -> Render () -> EventM EExpose ()
        updateCanvas canvas act = liftIO $ do
          win <- widgetGetDrawWindow canvas
          renderWithDrawable win act

setRed :: Render ()
setRed = do
  setSourceRGB 1 0 0



setFat :: Render ()
setFat = do
  setLineWidth 20
  setLineCap LineCapRound



drawSquare :: Double -> Double -> Render ()
drawSquare width height = do
  (x,y) <- getCurrentPoint
  lineTo (x+width) y
  lineTo (x+width) (y+height)
  lineTo x (y+height)
  closePath
  stroke



drawHCirc :: Double -> Double -> Double -> Render ()
drawHCirc x y radius = do
  arc x y radius 0 pi
  stroke



drawStr :: String -> Render ()
drawStr txt = do
  lay <- createLayout txt
  showLayout lay



drawStr_ :: String -> Render ()
drawStr_ txt = do
  lay <- liftIO $ do
    ctxt <- cairoCreateContext Nothing
    descr <- contextGetFontDescription ctxt
    descr `fontDescriptionSetSize` 20
    ctxt `contextSetFontDescription` descr
    layoutText ctxt txt
  showLayout lay
