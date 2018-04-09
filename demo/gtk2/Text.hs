import Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo.Matrix as M

boxText :: String -> Double -> Double -> Render ()
boxText text x y = do
  save

  lineWidth <- getLineWidth

  (TextExtents xb yb w h _ _) <- textExtents text

  rectangle (x + xb - lineWidth)
            (y + yb - lineWidth)
            (w + 2 * lineWidth)
            (h + 2 * lineWidth)
  stroke
  moveTo x y
  textPath text
  fillPreserve
  setSourceRGBA 0 0 1 0.5
  setLineWidth 3.0
  stroke

  restore

transpSurface :: Double -> Double -> Render ()
transpSurface w h = do
  save
  rectangle 0 0 w h
  setSourceRGBA 0 0 0 0
  setOperator OperatorSource
  fill
  restore

width = 400
height = 300

main :: IO ()
main = withImageSurface FormatARGB32 width height $ \surface -> do
  renderWith surface $ do
    setSourceRGB 0.0 0.0 0.0
    setLineWidth 2.0

    transpSurface (fromIntegral width) (fromIntegral height)

    selectFontFace "sans" FontSlantNormal FontWeightNormal
    setFontSize 40

    extents <- fontExtents
    let fontHeight = fontExtentsHeight extents

    boxText "Howdy, world!" 10 fontHeight

    translate 0 fontHeight

    save
    translate 10 fontHeight
    rotate (10.0 * pi / 180.0)
    boxText "Yay for Haskell!" 0 0
    restore

    translate 0 (3 * fontHeight)

    save
    setFontMatrix $ M.rotate ((-10.0) * pi / 180.0) $ M.scale 40.0 40.0 M.identity
    boxText "...and Cairo!" 10 fontHeight
    restore

  surfaceWriteToPNG surface "Text.png"

  return ()
