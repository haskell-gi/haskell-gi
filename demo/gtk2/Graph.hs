--
-- Author: Michael Sloan <mgsloan@gmail.com>
--
-- This code is in the public domain.
--
-- Based off Johan Bockgård's Drawing2.hs
--

import qualified Graphics.UI.Gtk as G
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Matrix as M

f x = sin (x*5) / (x*5)

main = graph f

graph :: (Double -> Double) -> IO ()
graph f = do
  G.initGUI
  window <- G.windowNew
  canvas <- G.drawingAreaNew
  G.windowSetResizable window False
  G.widgetSetSizeRequest window 600 600
  -- press any key to quit
  G.onKeyPress window $ const (do G.widgetDestroy window; return True)
  G.onDestroy window G.mainQuit
  G.onExpose canvas $ const $ render f canvas
  G.set window [G.containerChild G.:= canvas]
  G.widgetShowAll window
  G.mainGUI

render :: (Double -> Double) -> G.DrawingArea -> IO Bool
render f canvas = do
  win <- G.widgetGetDrawWindow canvas
  (width, height) <- G.widgetGetSize canvas
  G.renderWithDrawable win $ (prologue width height >> renderG f)
  return True

foreach :: (Monad m) => [a] -> (a -> m b) -> m [b]
foreach = flip mapM

deriv :: (Double -> Double) -> Double -> Double
deriv f x = ((f $ x + 0.05) - (f $ x - 0.05)) * 10

gen :: Double -> Double -> (Double -> Double) -> [Double]
gen v t f | v > t = []
gen v t f = v : (gen (f v) t f)

skipBy f = foldr (\x c -> if f x then c else x : c) []

falloff x = 0.25 * (x + 1.5) / ((x+0.5)^5 + 1)

renderG :: (Double -> Double) -> C.Render ()
renderG f = do
  C.moveTo (-5) (f (-5))
  sequence_ $ map (\d -> C.lineTo d $ f d) $ skipBy (isInfinite . f) [-4.9,-4.8..5]
  --Adaptive attempt (falloff func is what really needs work)
  --sequence_ $ map (\d -> C.lineTo d $ f d) $ skipBy (isInfinite . f) $ tail $ gen (-5) 5 (\x -> x + (falloff $ abs $ deriv (deriv f) x))
  C.stroke

-- Set up stuff
prologue wWidth wHeight = do
  let width   = 10
      height  = 10
      xmax    = width / 2
      xmin    = - xmax
      ymax    = height / 2
      ymin    = - ymax
      scaleX  = realToFrac wWidth  / width
      scaleY  = realToFrac wHeight / height

  -- style and color
  C.setLineCap C.LineCapRound
  C.setLineJoin C.LineJoinRound
  C.setLineWidth $ 1 / max scaleX scaleY

  -- Set up user coordinates
  C.scale scaleX scaleY
  -- center origin
  C.translate (width / 2) (height / 2)
  -- positive y-axis upwards
  let flipY = M.Matrix 1 0 0 (-1) 0 0
  C.transform flipY
  C.setSourceRGBA 0 0 0 1
  grid xmin xmax ymin ymax

-- Grid and axes
grid xmin xmax ymin ymax = do
  -- axes
  C.moveTo 0 ymin; C.lineTo 0 ymax; C.stroke
  C.moveTo xmin 0; C.lineTo xmax 0; C.stroke
  -- grid
  C.setDash [0.01, 0.99] 0
  foreach [xmin .. xmax] $ \ x ->
      do C.moveTo x ymin
         C.lineTo x ymax
         C.stroke
  C.setDash [] 0
