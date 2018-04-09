-- Example of an drawing graphics onto a canvas.
import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk as Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.EventM

main = do
  initGUI
  dia <- dialogNew
  -- in newer versions of GTK, 'size-request' is an invalid signal for Drawing areas,
  -- so the default size of the window will be changed instead.
  windowSetDefaultSize dia 400 400
  dialogAddButton dia stockOk ResponseOk
  contain <- dialogGetContentArea dia
  canvas <- drawingAreaNew
  ctxt <- cairoCreateContext Nothing
  text <- layoutEmpty ctxt
  text `layoutSetText` "Hello World."
  canvas `on` draw $ updateCanvas canvas text
  boxPackStart (castToBox contain) canvas PackGrow 0
  widgetShow canvas
  dialogRun dia
  return ()

updateCanvas :: WidgetClass widget => widget -> PangoLayout -> Render ()
updateCanvas canvas text = do
  width'  <- liftIO $ widgetGetAllocatedWidth  canvas
  height' <- liftIO $ widgetGetAllocatedHeight canvas
  let width  = realToFrac width'
      height = realToFrac height'

  setSourceRGB 1 0 0
  setLineWidth 20
  setLineCap LineCapRound
  setLineJoin LineJoinRound

  moveTo 30 30
  lineTo (width-30) (height-30)
  lineTo (width-30) 30
  lineTo 30 (height-30)
  stroke

  setSourceRGB 1 1 0
  setLineWidth 4

  save
  translate (width / 2) (height / 2)
  scale (width / 2) (height / 2)
  arc 0 0 1 (135 * pi/180) (225 * pi/180)
  restore
  stroke

  setSourceRGB 0 0 0
  moveTo 30 (realToFrac height / 4)
  rotate (pi/4)
  showLayout text


