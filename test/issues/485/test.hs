{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
-- | Regression test for
-- <https://github.com/haskell-gi/haskell-gi/issues/485>: the "glyphs"
-- field of PangoGlyphString was missing from the bindings.
--
-- The field is an array of PangoGlyphInfo, whose length is given by
-- another field ("num_glyphs"). Making it available required support
-- for such arrays in struct fields, and it is only useful if the size
-- of PangoGlyphInfo is right: the typelib gets it wrong, since
-- PangoGlyphInfo embeds a PangoGlyphVisAttr, which has bitfields, so
-- every element but the first would be read from the wrong place in
-- memory.
--
-- We check the field on glyph strings computed by Pango, on glyph
-- strings written from C (in test.c) and read from Haskell, and on
-- structs written from Haskell and read from C.
module Main (main) where

import Control.Monad (forM, forM_, unless, when)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Word (Word32)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import System.Exit (exitFailure)

import Data.GI.Base (AttrOp((:=)), get, set)
import Data.GI.Base.ManagedPtr (withManagedPtr, wrapBoxed)

import qualified GI.Pango as Pango
import qualified GI.PangoCairo as PangoCairo

foreign import ccall "issue485_make_glyph_string" c_make_glyph_string ::
  CInt -> IO (Ptr Pango.GlyphString)
foreign import ccall "issue485_make_glyphs" c_make_glyphs ::
  CInt -> IO (Ptr Pango.GlyphInfo)
foreign import ccall "issue485_glyph_has_pattern" c_glyph_has_pattern ::
  Ptr Pango.GlyphInfo -> CInt -> IO CInt
foreign import ccall "issue485_glyph_string_has_pattern" c_glyph_string_has_pattern ::
  Ptr Pango.GlyphString -> CInt -> IO CInt
foreign import ccall "issue485_vis_attr_equals" c_vis_attr_equals ::
  Ptr Pango.GlyphVisAttr -> Word32 -> Word32 -> IO CInt
foreign import ccall "issue485_geometry_equals" c_geometry_equals ::
  Ptr Pango.GlyphGeometry -> Int32 -> Int32 -> Int32 -> IO CInt
foreign import ccall "issue485_glyph_info_equals" c_glyph_info_equals ::
  Ptr Pango.GlyphInfo -> Word32 -> IO CInt

type Checker = String -> Bool -> IO ()

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  let check = checker failures

  pangoGlyphs check
  cWritesHaskellReads check
  haskellWritesCReads check

  n <- readIORef failures
  putStrLn (show n ++ " failures")
  unless (n == 0) exitFailure

checker :: IORef Int -> Checker
checker failures what ok = do
  putStrLn ((if ok then "ok   " else "FAIL ") ++ what)
  unless ok $ modifyIORef failures (+ 1)

-- | Read the glyphs of a glyph string, through both the getter and the
-- overloaded attribute, checking that they agree.
readGlyphs :: Checker -> Pango.GlyphString -> IO [Pango.GlyphInfo]
readGlyphs check glyphString = do
  glyphs <- fromMaybe [] <$> Pango.getGlyphStringGlyphs glyphString
  viaAttribute <- fromMaybe [] <$> get glyphString #glyphs
  ids <- mapM Pango.getGlyphInfoGlyph glyphs
  idsViaAttribute <- mapM Pango.getGlyphInfoGlyph viaAttribute
  check "the getter and the #glyphs attribute agree" (ids == idsViaAttribute)
  return glyphs

-- | The glyph strings computed by Pango when laying out text.
pangoGlyphs :: Checker -> IO ()
pangoGlyphs check = do
  putStrLn "* Glyph strings computed by Pango"
  fontMap <- PangoCairo.fontMapGetDefault
  context <- Pango.fontMapCreateContext fontMap
  layout <- Pango.layoutNew context

  -- Plain ASCII text, so that each character gives rise to exactly
  -- one glyph, laid out left to right.
  let text = "Hello, world"
  Pango.layoutSetText layout text (-1)
  Just line <- Pango.layoutGetLine layout 0
  runs <- Pango.getLayoutLineRuns line
  check "the layout line has runs" (not (null runs))
  forM_ runs $ \run -> do
    Just glyphString <- Pango.getGlyphItemGlyphs run
    Just item <- Pango.getGlyphItemItem run
    offset <- fromIntegral <$> Pango.getItemOffset item
    len <- Pango.getItemLength item
    let runText = T.take (fromIntegral len) (T.drop offset text)
    numGlyphs <- Pango.getGlyphStringNumGlyphs glyphString

    glyphs <- readGlyphs check glyphString
    check ("GlyphString.glyphs has num_glyphs = " ++ show numGlyphs ++ " elements")
          (length glyphs == fromIntegral numGlyphs)
    check ("one glyph per character in " ++ show runText)
          (length glyphs == T.length runText)

    -- Equal characters are rendered with the same glyph, and
    -- different characters with different glyphs.
    ids <- mapM Pango.getGlyphInfoGlyph glyphs
    let pairs = [ (c1 == c2, g1 == g2)
                | (c1, g1) <- zip (T.unpack runText) ids
                , (c2, g2) <- zip (T.unpack runText) ids ]
    check ("glyph ids " ++ show ids ++ " match the characters of " ++ show runText)
          (all (uncurry (==)) pairs)

    -- The position of each character computed by Pango agrees with
    -- the widths of the glyphs we read.
    widths <- forM glyphs $ \g ->
      Pango.getGlyphInfoGeometry g >>= Pango.getGlyphGeometryWidth
    analysis <- Pango.getItemAnalysis item
    positions <- forM [0 .. length glyphs - 1] $ \i ->
      Pango.glyphStringIndexToX glyphString runText len analysis
                                (fromIntegral i) False
    check ("pango_glyph_string_index_to_x " ++ show positions
           ++ " agrees with the glyph widths " ++ show widths)
          (positions == init (scanl (+) 0 widths))
    total <- Pango.glyphStringGetWidth glyphString
    check ("the glyph widths add up to pango_glyph_string_get_width ("
           ++ show (sum widths) ++ " == " ++ show total ++ ")")
          (sum widths == total)

    -- The bitfields in the PangoGlyphVisAttr embedded in each glyph:
    -- every glyph starts a cluster, since there is one glyph per
    -- character.
    clusterStarts <- forM glyphs $ \g ->
      Pango.getGlyphInfoAttr g >>= Pango.getGlyphVisAttrIsClusterStart
    check ("every glyph starts a cluster " ++ show clusterStarts)
          (all (== 1) clusterStarts)

-- | The values that test.c writes in glyph number i: (glyph, width,
-- x_offset, y_offset, is_cluster_start, is_color).
expectedGlyph :: Int -> (Word32, Int32, Int32, Int32, Word32, Word32)
expectedGlyph i = ( 1000 + fromIntegral i
                  , 1024 * (fromIntegral i + 1)
                  , 10 * fromIntegral i
                  , -10 * fromIntegral i
                  , if i `mod` 2 == 0 then 1 else 0
                  , if i `mod` 3 == 0 then 1 else 0 )

readGlyph :: Pango.GlyphInfo -> IO (Word32, Int32, Int32, Int32, Word32, Word32)
readGlyph g = do
  glyph <- Pango.getGlyphInfoGlyph g
  geometry <- Pango.getGlyphInfoGeometry g
  width <- Pango.getGlyphGeometryWidth geometry
  xOffset <- Pango.getGlyphGeometryXOffset geometry
  yOffset <- Pango.getGlyphGeometryYOffset geometry
  attr <- Pango.getGlyphInfoAttr g
  clusterStart <- Pango.getGlyphVisAttrIsClusterStart attr
  isColor <- Pango.getGlyphVisAttrIsColor attr
  return (glyph, width, xOffset, yOffset, clusterStart, isColor)

-- | Glyph strings written from C, and read from Haskell.
cWritesHaskellReads :: Checker -> IO ()
cWritesHaskellReads check = do
  putStrLn "* Glyph strings written from C, read from Haskell"
  let n = 7
  glyphString <- c_make_glyph_string (fromIntegral n) >>= wrapBoxed Pango.GlyphString
  numGlyphs <- Pango.getGlyphStringNumGlyphs glyphString
  check ("num_glyphs is " ++ show n) (numGlyphs == fromIntegral n)

  glyphs <- readGlyphs check glyphString
  check ("there are " ++ show n ++ " glyphs") (length glyphs == n)
  forM_ (zip [0 ..] glyphs) $ \(i, g) -> do
    values <- readGlyph g
    check ("glyph " ++ show i ++ " reads " ++ show values)
          (values == expectedGlyph i)
    -- The Haskell value is a copy of the C struct, check that it was
    -- copied completely and from the right place.
    same <- withManagedPtr g $ \p -> c_glyph_has_pattern p (fromIntegral i)
    check ("the Haskell copy of glyph " ++ show i ++ " is identical to the C one")
          (same /= 0)

  -- An empty glyph string has a NULL glyphs array.
  emptyString <- c_make_glyph_string 0 >>= wrapBoxed Pango.GlyphString
  empty <- Pango.getGlyphStringGlyphs emptyString
  check "an empty glyph string has no glyphs" (maybe True null empty)

-- | Structs written from Haskell, and read from C.
haskellWritesCReads :: Checker -> IO ()
haskellWritesCReads check = do
  putStrLn "* Structs written from Haskell, read from C"

  -- The bitfields in PangoGlyphVisAttr.
  forM_ [(c, s) | c <- [0, 1], s <- [0, 1]] $ \(clusterStart, isColor) -> do
    attr <- Pango.newZeroGlyphVisAttr
    Pango.setGlyphVisAttrIsClusterStart attr clusterStart
    Pango.setGlyphVisAttrIsColor attr isColor
    ok <- withManagedPtr attr $ \p -> c_vis_attr_equals p clusterStart isColor
    check ("GlyphVisAttr with is_cluster_start = " ++ show clusterStart
           ++ ", is_color = " ++ show isColor ++ " reads the same from C") (ok /= 0)
  attr <- Pango.newZeroGlyphVisAttr
  Pango.setGlyphVisAttrIsClusterStart attr 1
  Pango.setGlyphVisAttrIsColor attr 1
  Pango.setGlyphVisAttrIsClusterStart attr 0
  ok <- withManagedPtr attr $ \p -> c_vis_attr_equals p 0 1
  check "clearing is_cluster_start in GlyphVisAttr leaves is_color alone" (ok /= 0)

  geometry <- Pango.newZeroGlyphGeometry
  Pango.setGlyphGeometryWidth geometry 12345
  Pango.setGlyphGeometryXOffset geometry (-7)
  Pango.setGlyphGeometryYOffset geometry 42
  okGeometry <- withManagedPtr geometry $ \p -> c_geometry_equals p 12345 (-7) 42
  check "GlyphGeometry reads the same from C" (okGeometry /= 0)

  info <- Pango.newZeroGlyphInfo
  Pango.setGlyphInfoGlyph info 4242
  okInfo <- withManagedPtr info $ \p -> c_glyph_info_equals p 4242
  check "GlyphInfo reads the same from C" (okInfo /= 0)

  -- The fields of PangoGlyphString itself: we store an array of
  -- glyphs (allocated by C, and owned by the glyph string from now
  -- on, which will free it) and its length.
  let n = 5
  glyphString <- Pango.glyphStringNew
  glyphs <- c_make_glyphs (fromIntegral n)
  Pango.setGlyphStringGlyphs glyphString glyphs
  Pango.setGlyphStringNumGlyphs glyphString (fromIntegral n)
  okString <- withManagedPtr glyphString $ \p ->
                c_glyph_string_has_pattern p (fromIntegral n)
  check "the glyphs and num_glyphs set from Haskell read the same from C"
        (okString /= 0)
  values <- readGlyphs check glyphString >>= mapM readGlyph
  check "and they read back the same from Haskell"
        (values == map expectedGlyph [0 .. n - 1])

  -- Shrinking a glyph string written from C, using the overloaded
  -- attribute.
  fromC <- c_make_glyph_string 7 >>= wrapBoxed Pango.GlyphString
  set fromC [#numGlyphs := 3]
  okShrunk <- withManagedPtr fromC $ \p -> c_glyph_string_has_pattern p 3
  check "setting #numGlyphs from Haskell is seen from C" (okShrunk /= 0)
  shrunk <- readGlyphs check fromC
  when (length shrunk /= 3) $
    check ("reading the shrunk glyph string gives " ++ show (length shrunk)
           ++ " glyphs instead of 3") False
