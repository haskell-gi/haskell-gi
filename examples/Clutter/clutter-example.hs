{-# LANGUAGE OverloadedLabels, OverloadedStrings, ImplicitParams #-}
module Main where

import Data.GI.Base (get, AttrOp((:=), On), new)

import qualified GI.Clutter                         as Clutter
import qualified GI.Cogl                            as Cogl
import qualified GI.CoglPango                       as CoglPango

import Control.Monad (void)

textPaintCallback :: (?self :: Clutter.Text) => IO ()
textPaintCallback = do
    -- Get the PangoLayout that the text actor is going to paint
    layout <- #getLayout ?self

    -- Get the colour of the text, to extract the alpha component
    Just colour <- get ?self #color

    -- Composite the opacity so the shadow is correctly blended
    realOpacity <- #getPaintOpacity ?self >>= \v -> do
        alpha <- get colour #alpha
        pure $ v * alpha `div` 255 - 1

    -- Create a #cc colour and pre-multiply it
    coglColour <- new Cogl.Color []
    #initFrom4ub coglColour 0xcc 0xcc 0xcc realOpacity
    #premultiply coglColour

    -- Finally render the text layout at a given offset using the colour
    CoglPango.renderLayout layout 3 3 coglColour 0

keyPressCallback :: Clutter.KeyEvent -> IO Bool
keyPressCallback _ = do
    Clutter.mainQuit
    return Clutter.EVENT_STOP

main :: IO ()
main = do
    void $ Clutter.init Nothing

    stage <- new Clutter.Stage [ #title := "Hello World"
                               , On #destroy Clutter.mainQuit
                               , On #keyPressEvent keyPressCallback ]

    text <- new Clutter.Text [ #text := "Hello Haskell!!"
                             , #fontName := "Sans 64px"
                             , On #paint textPaintCallback
                             ]

    xAxisConstraint <- new Clutter.AlignConstraint [ #source := stage
                                                   , #alignAxis := Clutter.AlignAxisXAxis
                                                   , #factor := 0.5 ]

    yAxisConstraint <- new Clutter.AlignConstraint [ #source := stage
                                                   , #alignAxis := Clutter.AlignAxisYAxis
                                                   , #factor := 0.5 ]

    #addConstraint text xAxisConstraint
    #addConstraint text yAxisConstraint

    #addChild stage text
    #show stage

    Clutter.main
