import qualified Graphics.UI.SDL as SDL
import Graphics.Rendering.Cairo
import Foreign.Ptr ( castPtr )

demo1 :: Render ()
demo1 = do
  setSourceRGB 0 0 0
  moveTo 100 100
  lineTo 500 500
  stroke

demo2 :: Render ()
demo2 = do
  setSourceRGB 0 0 0
  moveTo 500 100
  lineTo 100 500
  stroke

main = SDL.withInit [ SDL.InitVideo ] $ do
  screen <- SDL.setVideoMode 600 600 32 [ SDL.SWSurface ]

  SDL.fillRect screen Nothing (SDL.Pixel maxBound)

  pixels <- fmap castPtr $ SDL.surfaceGetPixels screen

  canvas <- createImageSurfaceForData pixels FormatRGB24 600 600 (600 * 4)
  renderWith canvas demo1

  withImageSurfaceForData pixels FormatRGB24 600 600 (600 * 4) $ \canvas ->
    renderWith canvas demo2

  SDL.flip screen

  idle
  where
  idle = do
    e <- SDL.waitEvent
    case e of
         SDL.Quit  -> return ()
         otherwise -> idle
