import Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo.Matrix as M

ringPath :: Render ()
ringPath = do
  moveTo 200.86568 667.80795
  curveTo 110.32266 562.62134 122.22863 403.77940 227.41524 313.23637
  curveTo 332.60185 222.69334 491.42341 234.57563 581.96644 339.76224
  curveTo 672.50948 444.94884 660.64756 603.79410 555.46095 694.33712
  curveTo 450.27436 784.88016 291.40871 772.99456 200.86568 667.80795
  closePath
  moveTo 272.14411 365.19927
  curveTo 195.64476 431.04875 186.97911 546.57972 252.82859 623.07908
  curveTo 318.67807 699.57844 434.23272 708.22370 510.73208 642.37422
  curveTo 587.23144 576.52474 595.85301 460.99047 530.00354 384.49112
  curveTo 464.15406 307.99176 348.64347 299.34979 272.14411 365.19927
  closePath

starPath :: Render ()
starPath = do
  transform (M.Matrix 0.647919 (-0.761710) 0.761710 0.647919 (-208.7977) 462.0608)
  moveTo 505.80857 746.23606
  lineTo 335.06870 555.86488
  lineTo 91.840384 635.31360
  lineTo 282.21157 464.57374
  lineTo 202.76285 221.34542
  lineTo 373.50271 411.71660
  lineTo 616.73103 332.26788
  lineTo 426.35984 503.00775
  lineTo 505.80857 746.23606
  closePath

fillRing :: Render ()
fillRing = do
  save
  translate (-90) (-205)
  ringPath
  setSourceRGBA 1.0 0.0 0.0 0.75
  fill
  restore

fillStar :: Render ()
fillStar = do
  save
  translate (-90) (-205)
  starPath
  setSourceRGBA 0.0 0.0 ((fromIntegral 0xae) / (fromIntegral 0xff)) 0.55135137
  fill
  restore

clipToTopAndBottom :: Int -> Int -> Render ()
clipToTopAndBottom width height = do
  moveTo 0 0
  lineTo (fromIntegral width) 0.0
  lineTo 0.0 (fromIntegral height)
  lineTo (fromIntegral width) (fromIntegral height)
  closePath
  clip
  newPath

clipToLeftAndRight :: Int -> Int -> Render ()
clipToLeftAndRight width height = do
  moveTo 0 0
  lineTo 0.0 (fromIntegral height)
  lineTo (fromIntegral width) 0.0
  lineTo (fromIntegral width) (fromIntegral height)
  closePath
  clip
  newPath

starAndRing :: Int -> Int -> Render ()
starAndRing width height = do
  setOperator OperatorClear
  paint

  setOperator OperatorAdd

  renderWithSimilarSurface ContentColorAlpha width height $ \ringOverStar -> do
    renderWith ringOverStar $ do
      clipToTopAndBottom width height
      fillStar
      fillRing
    setSourceSurface ringOverStar 0 0
    paint

  renderWithSimilarSurface ContentColorAlpha width height $ \starOverRing -> do
    renderWith starOverRing $ do
      clipToLeftAndRight width height
      fillRing
      fillStar
    setSourceSurface starOverRing 0 0
    paint

main :: IO ()
main = do
  withImageSurface FormatARGB32 width height $ \result -> do
    renderWith result $ starAndRing width height
    surfaceWriteToPNG result "StarAndRing.png"
  putStrLn "wrote StarAndRing.png"
  withPDFSurface "StarAndRing.pdf" (fromIntegral width) (fromIntegral height)
    (flip renderWith $ starAndRing width height >> showPage)
  putStrLn "wrote StarAndRing.pdf"
  withPSSurface "StarAndRing.ps" (fromIntegral width) (fromIntegral height)
    (flip renderWith $ starAndRing width height >> showPage)
  putStrLn "wrote StarAndRing.ps"
  withSVGSurface "StarAndRing.svg" (fromIntegral width) (fromIntegral height)
    (flip renderWith $ starAndRing width height)
  putStrLn "wrote StarAndRing.svg"

    where width = 600
          height = 600
