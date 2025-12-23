{-# LANGUAGE PatternSynonyms, OverloadedStrings #-}

-- original author:
--    Mirco "MacSlow" Mueller <macslow@bangang.de>
--
-- created:
--    10.1.2006 (or so)
--
-- http://www.gnu.org/licenses/licenses.html#GPL
--
-- ported to Haskell by:
--    Duncan Coutts <duncan.coutts@worc.ox.ac.uk>
--
-- updated to GTK 3 by Catherine Holloway
-- converted to Haskell GI by Kilian Kilger
--
import qualified GI.Cairo
import qualified GI.Gdk as GDK
import qualified GI.Gtk as GTK  
import GI.GLib (pattern PRIORITY_DEFAULT, timeoutAdd)
import GI.Cairo.Render.Connector (renderWithContext)
import GI.Cairo.Render
import Data.Time
import Control.Monad (when)
import Data.Maybe (isJust)
import Data.IORef
import Data.Text as Text
import Data.Maybe (fromMaybe)

drawClockBackground :: GTK.IsWidget widget => widget -> Bool -> Render ()
drawClockBackground canvas quality = do

  width  <- liftIO $ GTK.widgetGetAllocatedWidth  canvas
  height <- liftIO $ GTK.widgetGetAllocatedHeight canvas
  save
  scale (fromIntegral width) (fromIntegral height)

  save
  setOperator OperatorOver
  when quality drawDropShadow
  drawClockFace quality
  restore

  translate 0.5 0.5
  scale 0.4 0.4
  setSourceRGB 0.16 0.18 0.19
  setLineWidth (1.5/60)
  setLineCap LineCapRound
  setLineJoin LineJoinRound
  drawHourMarks

  restore

drawClockHands :: GTK.IsWidget widget => widget -> Bool -> Render ()
drawClockHands canvas quality = do

  width  <- liftIO $ GTK.widgetGetAllocatedWidth  canvas
  height <- liftIO $ GTK.widgetGetAllocatedHeight canvas
  save
  scale (fromIntegral width) (fromIntegral height)

  translate 0.5 0.5
  scale 0.4 0.4
  setSourceRGB 0.16 0.18 0.19
  setLineWidth (1.5/60)
  setLineCap LineCapRound
  setLineJoin LineJoinRound

  time <- liftIO (localTimeOfDay . zonedTimeToLocalTime <$> getZonedTime)
  let hours   = fromIntegral (todHour time `mod` 12)
      minutes = fromIntegral (todMin time)
      seconds = realToFrac (todSec time)

  drawHourHand quality hours minutes seconds
  drawMinuteHand quality minutes seconds
  drawSecondHand quality seconds

  restore

drawClockForeground :: Bool -> Int -> Int -> Render ()
drawClockForeground quality width height = do
  scale (fromIntegral width) (fromIntegral height)

  save
  translate 0.5 0.5
  scale 0.4 0.4
  setSourceRGB 0.16 0.18 0.19
  setLineWidth (1.5/60)
  setLineCap LineCapRound
  setLineJoin LineJoinRound

  when quality drawInnerShadow
  when quality drawReflection
  drawFrame quality
  restore

drawDropShadow =
  withRadialPattern 0.55 0.55 0.25 0.5 0.5 0.525 $ \p -> do
    patternAddColorStopRGBA p 0    0     0     0     0.811
    patternAddColorStopRGBA p 0.64 0.345 0.345 0.345 0.317
    patternAddColorStopRGBA p 0.84 0.713 0.713 0.713 0.137
    patternAddColorStopRGBA p 1    1     1     1     0
    patternSetFilter p FilterFast
    setSource p
    arc 0.5 0.5 (142/150) 0 (pi*2)
    fill

drawClockFace True =
  withLinearPattern 0.5 0 0.5 1 $ \p -> do
    patternAddColorStopRGB p 0 0.91 0.96 0.93
    patternAddColorStopRGB p 1 0.65 0.68 0.68
    patternSetFilter p FilterFast
    setSource p
    translate 0.5 0.5
    arc 0 0 (60/150) 0 (pi*2)
    fill
drawClockFace False = do
  setSourceRGB 0.78 0.82 0.805
  translate 0.5 0.5
  arc 0 0 (60/150) 0 (pi*2)
  fill

drawHourMarks = do
  save
  forM_ [1..12] $ \_ -> do
    rotate (pi/6)
    moveTo (4.5/6) 0
    lineTo (5.0/6) 0
  stroke
  restore

forM_ = flip mapM_

drawHourHand quality hours minutes seconds = do
  save
  rotate (-pi/2)
  setLineCap LineCapSquare
  setLineJoin LineJoinMiter
  rotate ( (pi/6) * hours
         + (pi/360) * minutes
         + (pi/21600) * seconds)

  -- hour hand's shadow
  when quality $ do
    setLineWidth (1.75/60)
    setOperator OperatorAtop
    setSourceRGBA 0.16 0.18 0.19 0.125
    moveTo (-2/15 + 0.025) 0.025
    lineTo (7/15 + 0.025) 0.025
    stroke

  -- the hand itself
  setLineWidth (1/60)
  setOperator OperatorOver
  setSourceRGB 0.16 0.18 0.19
  moveTo (-2/15) 0
  lineTo (7/15) 0
  stroke
  restore

drawMinuteHand quality minutes seconds = do
  save
  rotate (-pi/2)
  setLineCap LineCapSquare
  setLineJoin LineJoinMiter
  rotate ( (pi/30) * minutes
         + (pi/1800) * seconds)

  -- minute hand's shadow
  when quality $ do
    setLineWidth (1.75/60)
    setOperator OperatorAtop
    setSourceRGBA 0.16 0.18 0.19 0.125
    moveTo (-16/75 - 0.025) (-0.025)
    lineTo (2/3 - 0.025)    (-0.025)
    stroke

  -- the minute hand itself
  setLineWidth (1/60)
  setOperator OperatorOver
  setSourceRGB 0.16 0.18 0.19
  moveTo (-16/75) 0
  lineTo (2/3) 0
  stroke
  restore

drawSecondHand quality seconds = do
  save
  rotate (-pi/2)
  setLineCap LineCapSquare
  setLineJoin LineJoinMiter
  rotate (seconds * pi/30);

  -- shadow of second hand-part
  when quality $ do
    setOperator  OperatorAtop
    setSourceRGBA 0.16 0.18 0.19 0.125
    setLineWidth  (1.3125 / 60)
    moveTo (-1.5/5 + 0.025) 0.025
    lineTo (3/5 + 0.025) 0.025
    stroke

  -- second hand
  setOperator OperatorOver
  setSourceRGB 0.39 0.58 0.77
  setLineWidth (0.75/60)
  moveTo (-1.5/5) 0
  lineTo (3/5) 0
  stroke

  arc 0 0 (1/20) 0 (pi*2)
  fill
  arc (63/100) 0 (1/35) 0 (pi*2)
  stroke
  setLineWidth  (1/100)
  moveTo  (10/15) 0
  lineTo  (12/15) 0
  stroke
  setSourceRGB  0.31 0.31 0.31
  arc  0 0 (1/25) 0 (pi*2)
  fill
  restore

drawInnerShadow = do
  save
  setOperator OperatorOver
  arc 0 0 (142/150) 0 (pi*2)
  clip
  withRadialPattern 0.3 0.3 0.1 0 0 0.95 $ \p -> do
    patternAddColorStopRGBA p 0    1     1     1     0
    patternAddColorStopRGBA p 0.64 0.713 0.713 0.713 0.137
    patternAddColorStopRGBA p 0.84 0.345 0.345 0.345 0.317
    patternAddColorStopRGBA p 1    0     0     0     0.811
    patternSetFilter p FilterFast
    setSource p
    arc 0 0 (142/150) 0 (pi*2)
    fill
  restore

drawReflection = do
  save
  arc 0 0 (142/150) 0 (pi*2)
  clip
  rotate (-75 * pi/180)
  setSourceRGBA 0.87 0.9 0.95 0.25
  moveTo (-1) (-1)
  lineTo 1 (-1)
  lineTo 1 1
  curveTo 1 0.15 (-0.15) (-1) (-1) (-1)
  fill
  moveTo (-1) (-1)
  lineTo (-1) 1
  lineTo 1 1
  curveTo (-0.5) 1 (-1) 0.5 (-1) (-1)
  fill
  restore

drawFrame True = do
  save
  withRadialPattern (-0.1) (-0.1) 0.8 0 0 1.5 $ \p -> do
    patternAddColorStopRGB p 0   0.4  0.4  0.4
    patternAddColorStopRGB p 0.2 0.95 0.95 0.95
    patternSetFilter p FilterFast
    setSource p
    setLineWidth (10/75)
    arc 0 0 (142/150) 0 (pi*2)
    stroke

  withRadialPattern (-0.1) (-0.1) 0.8 0 0 1.5 $ \p -> do
    patternAddColorStopRGB p 0   0.9  0.9  0.9
    patternAddColorStopRGB p 0.2 0.35 0.35 0.35
    patternSetFilter p FilterFast
    setSource p
    setLineWidth (10/75)
    arc 0 0 (150/150) 0 (pi*2)
    stroke
  restore
drawFrame False = do
  save
  setSourceRGB 0 0 0
  setLineWidth (10/75)
  arc 0 0 1 0 (pi*2)
  stroke
  restore

initialSize :: Int
initialSize = 256

drawCanvasHandler :: GTK.IsWidget widget => widget -> Render Bool
drawCanvasHandler widget =  
  do drawClockBackground widget True
     drawClockHands widget True 
     return True

main :: IO ()
main = do
  GTK.init Nothing
  window <- GTK.windowNew GTK.WindowTypeToplevel 
  GTK.windowSetPosition window GTK.WindowPositionCenterAlways

  GTK.widgetSetAppPaintable window True

  GTK.windowSetDefaultSize window (fromIntegral initialSize) 
                                  (fromIntegral initialSize)

  geometry <- GDK.newZeroGeometry
  GDK.setGeometryMaxWidth  geometry 512
  GDK.setGeometryMaxHeight geometry 512
  GDK.setGeometryMinWidth  geometry 32
  GDK.setGeometryMinHeight geometry 32 
  GDK.setGeometryMinAspect geometry 1
  GDK.setGeometryMaxAspect geometry 1

  GTK.windowSetGeometryHints window (Just window) (Just geometry) []

  GTK.onWidgetKeyPressEvent window $ \keyPressInfo -> do 
    keyVal <- GDK.getEventKeyKeyval keyPressInfo
    keyName <- fromMaybe Text.empty <$> GDK.keyvalName keyVal
    case Text.unpack keyName of
      "Escape" -> do GTK.mainQuit
                     return True
      _        -> return False

  GTK.onWidgetButtonPressEvent window $ \button -> do
    btnNo <- GDK.getEventButtonButton button
    x     <- GDK.getEventButtonX button  
    y     <- GDK.getEventButtonY button
    time  <- GDK.getEventButtonTime button
    case btnNo of
      1  -> do GTK.windowBeginMoveDrag window 1 (round x) (round y) time  -- left button
               return True
      2  -> do GTK.windowBeginResizeDrag window GDK.WindowEdgeSouthEast 2 -- middle button
                                         (round x) (round y) time 
               return True
      _  -> return False 

  canvas <- GTK.drawingAreaNew
  GTK.containerAdd window canvas 

  GTK.setWindowDecorated window False
  GTK.setWindowResizable window True
  GTK.setWindowTitle window (pack "Cairo Clock")

  GTK.onWidgetDraw canvas $ renderWithContext (drawCanvasHandler canvas) 

  GTK.widgetShowAll window
  timeoutAdd GI.GLib.PRIORITY_DEFAULT 1000 (GTK.widgetQueueDraw window >> return True)
  GTK.main
