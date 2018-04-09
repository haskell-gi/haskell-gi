{-# LANGUAGE OverloadedStrings #-}
-- An example of Cairo drawing to an SDL screen.
-- updated to GTK 3 and SDL2 by Catherine Holloway
-- This example uses SDL 2+ instead of SDL 1 in the gtk2 version, and requires
-- the haskell package sdl2.

import SDL
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

screenWidth = 600
screenHeight = 600

main = do
  SDL.initialize [ SDL.InitVideo ]
  window <-
    SDL.createWindow
      "SDL / Cairo Example"
      SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight}
  SDL.showWindow window
  screenSurface <- SDL.getWindowSurface window
  let white = V4 maxBound maxBound maxBound maxBound
  SDL.surfaceFillRect screenSurface Nothing white
  pixels <- fmap castPtr $ surfacePixels screenSurface

  canvas <- createImageSurfaceForData pixels FormatRGB24 600 600 (600 * 4)
  renderWith canvas demo1

  withImageSurfaceForData pixels FormatRGB24 600 600 (600 * 4) $ \canvas ->
    renderWith canvas demo2


  SDL.updateWindowSurface window
  idle
  where
  idle = do
    events <- SDL.pollEvents
    let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
    case quit of
         True -> return ()
         otherwise -> idle
