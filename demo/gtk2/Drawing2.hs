--
-- Author: Johan Bockgård <bojohan@dd.chalmers.se>
--
-- This code is in the public domain.
--

import qualified Graphics.UI.Gtk as G
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Matrix as M


windowWidth, windowHeight :: Int
windowWidth   = 500
windowHeight  = 500

-- Write image to file
writePng :: IO ()
writePng =
  C.withImageSurface C.FormatARGB32 width height $ \ result -> do
      C.renderWith result $ example width height
      C.surfaceWriteToPNG result "Draw.png"
  where width  = windowWidth
        height = windowHeight

-- Display image in window
main = do
  G.initGUI
  window <- G.windowNew
  canvas <- G.drawingAreaNew
  -- fix size
  --   G.windowSetResizable window False
  G.widgetSetSizeRequest window windowWidth windowHeight
  -- press any key to quit
  G.onKeyPress window $ const (do G.widgetDestroy window; return True)
  G.onDestroy window G.mainQuit
  G.onExpose canvas $ const (updateCanvas canvas)
  G.set window [G.containerChild G.:= canvas]
  G.widgetShowAll window
  G.mainGUI

updateCanvas :: G.DrawingArea -> IO Bool
updateCanvas canvas = do
  win <- G.widgetGetDrawWindow canvas
  (width, height) <- G.widgetGetSize canvas
  G.renderWithDrawable win $
      example width height
  return True

----------------------------------------------------------------

foreach :: (Monad m) => [a] -> (a -> m b) -> m [b]
foreach = flip mapM

keepState render = do
  C.save
  render
  C.restore

drawCircle x y r = do
  C.arc x y r 0 (2 * pi)
  fillStroke

drawRectangle x y w h = do
  C.rectangle x y w h
  fillStroke

stroke =
  keepState $ do
  C.setSourceRGBA 0 0 0 0.7
  C.stroke

fillStroke = do
  C.fillPreserve
  stroke

----------------------------------------------------------------

-- Example

example width height = do
  prologue width height
  example1

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
  C.setSourceRGBA 0.5 0.7 0.5 0.5

  -- Set up user coordinates
  C.scale scaleX scaleY
  -- center origin
  C.translate (width / 2) (height / 2)
  -- positive y-axis upwards
  let flipY = M.Matrix 1 0 0 (-1) 0 0
  C.transform flipY

  grid xmin xmax ymin ymax


-- Grid and axes
grid xmin xmax ymin ymax =
  keepState $ do
  C.setSourceRGBA 0 0 0 0.7
  -- axes
  C.moveTo 0 ymin; C.lineTo 0 ymax; C.stroke
  C.moveTo xmin 0; C.lineTo xmax 0; C.stroke
  -- grid
  C.setDash [0.01, 0.99] 0
  foreach [xmin .. xmax] $ \ x ->
      do C.moveTo x ymin
         C.lineTo x ymax
         C.stroke

example1 = do
  -- circles
  drawCircle 0 0 1
  drawCircle 2 2 3
  -- a bunch of rectangles
  keepState $
    foreach [1 .. 5] $ \ _ ->
        do drawRectangle 0 1 2 3
           C.rotate (pi/8)
  -- some cute stuff
  thought
  apple
  snake

thought =
  keepState $ do
  C.scale 0.04 0.04
  C.translate (200) (380)
  C.rotate pi
  C.setSourceRGBA 0.5 0.5 1 0.7
  C.setLineWidth 1
  image
  fillStroke
  where
    m = C.moveTo
    c = C.curveTo
    z = C.closePath
    image = do
        m 184 327
        c 176 327 170 332 168 339
        c 166 333 160 329 153 329
        c 147 329 141 333 138 339
        c 137 339 136 338 134 338
        c 125 338 118 345 118 354
        c 118 363 125 371 134 371
        c 137 371 140 370 142 368
        c 142 368 142 368 142 369
        c 142 377 149 385 158 385
        c 162 385 166 383 168 381
        c 171 386 176 390 183 390
        c 188 390 193 387 196 383
        c 198 384 201 385 204 385
        c 212 385 220 378 220 369
        c 222 371 225 372 228 372
        c 237 372 244 364 244 355
        c 244 346 237 339 228 339
        c 227 339 226 339 225 340
        c 223 332 217 327 209 327
        c 204 327 199 330 196 333
        c 193 330 189 327 184 327
        z
        m 164 387
        c 158 387 153 391 153 397
        c 153 402 158 407 164 407
        c 170 407 174 402 174 397
        c 174 391 170 387 164 387
        z
        m 152 408
        c 149 408 146 411 146 414
        c 146 417 149 420 152 420
        c 155 420 158 417 158 414
        c 158 411 155 408 152 408
        z
        m 143 422
        c 141 422 139 424 139 426
        c 139 428 141 429 143 429
        c 144 429 146 428 146 426
        c 146 424 144 422 143 422
        z

apple =
  keepState $ do
  C.scale 0.05 0.05
  C.translate (1110) (220)
  C.rotate pi
  C.setLineWidth 0.5
  C.setSourceRGBA 0 0 0 0.7
  image1
  fillStroke
  C.setSourceRGBA 1 0 0 0.7
  image2
  fillStroke
  where
    m = C.moveTo
    c = C.curveTo
    z = C.closePath
    l = C.lineTo
    image1 = do
        m 1149 245
        l 1156 244
        l 1155 252
        l 1149 245
        z
    image2 = do
        m 1151 249
        c 1145 249 1140 254 1140 261
        c 1140 268 1145 273 1151 273
        c 1152 273 1153 273 1154 272
        c 1156 273 1157 273 1158 273
        c 1164 273 1169 268 1169 261
        c 1169 254 1164 249 1158 249
        c 1157 249 1156 249 1154 250
        c 1153 249 1152 249 1151 249
        z

snake =
  keepState $ do
  C.scale 0.04 0.04
  C.translate (150) (220)
  C.rotate pi
  C.setLineWidth 0.5
  C.setSourceRGBA 0.1 0.1 0 0.7
  image
  fillStroke
  where
    m = C.moveTo
    c = C.curveTo
    z = C.closePath
    l = C.lineTo
    image = do
        m 146 320
        c 143 308 130 314 123 319
        c 115 324 108 311 100 314
        c  93 317  92 319  81 318
        c  76 318  60 309  60 320
        c  60 328  73 321  82 323
        c  94 326  98 317 106 320
        c 113 323 120 330 128 323
        c 133 318 142 312 146 320
        l 146 320
        z

----------------------------------------------------------------
