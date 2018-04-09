{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.Types
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see cairo/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Haskell bindings to the cairo types.
-----------------------------------------------------------------------------

-- #hide
module Graphics.Rendering.Cairo.Types (
    PixelData
  , Matrix(Matrix), MatrixPtr
  , Cairo(Cairo), unCairo
  , Surface(Surface), withSurface, mkSurface, manageSurface
  , Pattern(Pattern), unPattern
  , Status(..)
  , Operator(..)
  , Antialias(..)
  , FillRule(..)
  , LineCap(..)
  , LineJoin(..)
  , ScaledFont(..), unScaledFont
  , FontFace(..), unFontFace
  , Glyph, unGlyph
  , TextExtentsPtr
  , TextExtents(..)
  , FontExtentsPtr
  , FontExtents(..)
  , FontSlant(..)
  , FontWeight(..)
  , SubpixelOrder(..)
  , HintStyle(..)
  , HintMetrics(..)
  , FontOptions(..), withFontOptions, mkFontOptions
  , Path(..), unPath
#if CAIRO_CHECK_VERSION(1,10,0)
  , RectangleInt(..)
  , RegionOverlap(..)
  , Region(..), withRegion, mkRegion
#endif
  , Content(..)
  , Format(..)
  , Extend(..)
  , Filter(..)

  , cIntConv
  , cFloatConv
  , cFromBool
  , cToBool
  , cToEnum
  , cFromEnum
  , peekFloatConv
  , withFloatConv

  ) where

{#import Graphics.Rendering.Cairo.Matrix#}

import Foreign hiding (rotate)
import Foreign.C

import Control.Monad (liftM)

{#context lib="cairo" prefix="cairo"#}

type PixelData = Ptr CUChar

-- not visible
{#pointer *cairo_t as Cairo newtype#}
unCairo (Cairo x) = x

-- | The medium to draw on.
{#pointer *surface_t as Surface foreign newtype#}
withSurface (Surface x) = withForeignPtr x

mkSurface :: Ptr Surface -> IO Surface
mkSurface surfacePtr = do
  surfaceForeignPtr <- newForeignPtr_ surfacePtr
  return (Surface surfaceForeignPtr)

manageSurface :: Surface -> IO ()
manageSurface (Surface surfaceForeignPtr) = do
  addForeignPtrFinalizer surfaceDestroy surfaceForeignPtr

foreign import ccall unsafe "&cairo_surface_destroy"
  surfaceDestroy :: FinalizerPtr Surface

-- | Patterns can be simple solid colors, various kinds of gradients or
-- bitmaps. The current pattern for a 'Render' context is used by the 'stroke',
-- 'fill' and paint operations. These operations composite the current pattern
-- with the target surface using the currently selected 'Operator'.
--
{#pointer *pattern_t as Pattern newtype#}
unPattern (Pattern x) = x

-- | Cairo status.
--
-- * 'Status' is used to indicate errors that can occur when using
--   Cairo. In some cases it is returned directly by functions. When using
--   'Graphics.Rendering.Cairo.Render', the last error, if any, is stored
--   in the monad and can be retrieved with 'Graphics.Rendering.Cairo.status'.
--
{#enum status_t as Status {underscoreToCase} deriving(Eq,Show)#}

-- | Composition operator for all drawing operations.
--
{#enum operator_t as Operator {underscoreToCase} deriving(Eq,Show)#}

-- | Specifies the type of antialiasing to do when rendering text or shapes
--
-- ['AntialiasDefault']  Use the default antialiasing for the subsystem
-- and target device.
--
-- ['AntialiasNone']  Use a bilevel alpha mask.
--
-- ['AntialiasGray']  Perform single-color antialiasing (using shades of
-- gray for black text on a white background, for example).
--
-- ['AntialiasSubpixel']  Perform antialiasing by taking advantage of
-- the order of subpixel elements on devices such as LCD panels.
--
{#enum antialias_t as Antialias {underscoreToCase} deriving(Eq,Show)#}

-- | Specify how paths are filled.
--
-- * For both fill rules, whether or not a point is included in the fill is
--   determined by taking a ray from that point to infinity and looking at
--   intersections with the path. The ray can be in any direction, as long
--   as it doesn't pass through the end point of a segment or have a tricky
--   intersection such as intersecting tangent to the path. (Note that
--   filling is not actually implemented in this way. This is just a
--   description of the rule that is applied.)
--
-- ['FillRuleWinding']  If the path crosses the ray from left-to-right,
--   counts +1. If the path crosses the ray from right to left, counts -1.
--   (Left and right are determined from the perspective of looking along
--   the ray from the starting point.) If the total count is non-zero, the
--   point will be filled.
--
-- ['FillRuleEvenOdd']  Counts the total number of intersections,
--   without regard to the orientation of the contour. If the total number
--   of intersections is odd, the point will be filled.
--
{#enum fill_rule_t as FillRule {underscoreToCase} deriving(Eq,Show)#}

-- | Specify line endings.
--
-- ['LineCapButt'] Start(stop) the line exactly at the start(end) point.
--
-- ['LineCapRound'] Use a round ending, the center of the circle is the
--   end point.
--
-- ['LineCapSquare'] Use squared ending, the center of the square is the
--   end point
--
{#enum line_cap_t as LineCap {underscoreToCase} deriving(Eq,Show)#}

-- | Specify how lines join.
--
{#enum line_join_t as LineJoin {underscoreToCase} deriving(Eq,Show)#}

{#pointer *scaled_font_t as ScaledFont newtype#}
unScaledFont (ScaledFont x) = x

{#pointer *font_face_t as FontFace newtype#}
unFontFace (FontFace x) = x

{#pointer *glyph_t as Glyph newtype#}
unGlyph (Glyph x) = x

{#pointer *text_extents_t as TextExtentsPtr -> TextExtents#}

-- | Specify the extents of a text.
data TextExtents = TextExtents {
    textExtentsXbearing :: Double
  , textExtentsYbearing :: Double
  , textExtentsWidth    :: Double
  , textExtentsHeight   :: Double
  , textExtentsXadvance :: Double
  , textExtentsYadvance :: Double
  }

instance Storable TextExtents where
  sizeOf _ = {#sizeof text_extents_t#}
  alignment _ = alignment (undefined :: CDouble)
  peek p = do
    x_bearing <- {#get text_extents_t->x_bearing#} p
    y_bearing <- {#get text_extents_t->y_bearing#} p
    width     <- {#get text_extents_t->width#}     p
    height    <- {#get text_extents_t->height#}    p
    x_advance <- {#get text_extents_t->x_advance#} p
    y_advance <- {#get text_extents_t->y_advance#} p
    return $ TextExtents (cFloatConv x_bearing) (cFloatConv y_bearing)
                         (cFloatConv width)     (cFloatConv height)
                         (cFloatConv x_advance) (cFloatConv y_advance)
  poke p (TextExtents x_bearing y_bearing width height x_advance y_advance) = do
    {#set text_extents_t->x_bearing#} p (cFloatConv x_bearing)
    {#set text_extents_t->y_bearing#} p (cFloatConv y_bearing)
    {#set text_extents_t->width#}     p (cFloatConv width)
    {#set text_extents_t->height#}    p (cFloatConv height)
    {#set text_extents_t->x_advance#} p (cFloatConv x_advance)
    {#set text_extents_t->y_advance#} p (cFloatConv y_advance)
    return ()

{#pointer *font_extents_t as FontExtentsPtr -> FontExtents#}

-- | Result of querying the font extents.
data FontExtents = FontExtents {
    fontExtentsAscent      :: Double
  , fontExtentsDescent     :: Double
  , fontExtentsHeight      :: Double
  , fontExtentsMaxXadvance :: Double
  , fontExtentsMaxYadvance :: Double
  }

instance Storable FontExtents where
  sizeOf _ = {#sizeof font_extents_t#}
  alignment _ = alignment (undefined :: CDouble)
  peek p = do
    ascent        <- {#get font_extents_t->ascent#}        p
    descent       <- {#get font_extents_t->descent#}       p
    height        <- {#get font_extents_t->height#}        p
    max_x_advance <- {#get font_extents_t->max_x_advance#} p
    max_y_advance <- {#get font_extents_t->max_y_advance#} p
    return $ FontExtents (cFloatConv ascent) (cFloatConv descent) (cFloatConv height)
                         (cFloatConv max_x_advance) (cFloatConv max_y_advance)
  poke p (FontExtents ascent descent height max_x_advance max_y_advance) = do
    {#set font_extents_t->ascent#}        p (cFloatConv ascent)
    {#set font_extents_t->descent#}       p (cFloatConv descent)
    {#set font_extents_t->height#}        p (cFloatConv height)
    {#set font_extents_t->max_x_advance#} p (cFloatConv max_x_advance)
    {#set font_extents_t->max_y_advance#} p (cFloatConv max_y_advance)
    return ()

-- | Specify font slant.
{#enum font_slant_t as FontSlant {underscoreToCase} deriving(Eq,Show)#}

-- | Specify font weight.
{#enum font_weight_t as FontWeight {underscoreToCase} deriving(Eq,Show)#}

-- | The subpixel order specifies the order of color elements within each pixel
-- on the display device when rendering with an antialiasing mode of
-- 'AntialiasSubpixel'.
--
-- ['SubpixelOrderDefault'] Use the default subpixel order for for the
--                          target device
--
-- ['SubpixelOrderRgb']     Subpixel elements are arranged horizontally
--                          with red at the left
--
-- ['SubpixelOrderBgr']     Subpixel elements are arranged horizontally
--                          with blue at the left
--
-- ['SubpixelOrderVrgb']    Subpixel elements are arranged vertically
--                          with red at the top
--
-- ['SubpixelOrderVbgr']    Subpixel elements are arranged vertically
--                          with blue at the top
--
{#enum subpixel_order_t as SubpixelOrder {underscoreToCase} deriving(Eq,Show)#}

-- | Specifies the type of hinting to do on font outlines.
--
-- Hinting is the process of fitting outlines to the pixel grid in order to
-- improve the appearance of the result. Since hinting outlines involves
-- distorting them, it also reduces the faithfulness to the original outline
-- shapes. Not all of the outline hinting styles are supported by all font
-- backends.
--
-- ['HintStyleDefault']  Use the default hint style for for font backend and
--                       target device
--
-- ['HintStyleNone']     Do not hint outlines
--
-- ['HintStyleSlight']   Hint outlines slightly to improve contrast while
--                       retaining good fidelity to the original shapes.
--
-- ['HintStyleMedium']   Hint outlines with medium strength giving a compromise
--                       between fidelity to the original shapes and contrast
--
-- ['HintStyleFull']     Hint outlines to maximize contrast
--
{#enum hint_style_t as HintStyle {underscoreToCase}#}

-- | Specifies whether to hint font metrics.
--
-- Hinting font metrics means quantizing them so that they are integer values
-- in device space. Doing this improves the consistency of letter and line
-- spacing, however it also means that text will be laid out differently at
-- different zoom factors.
--
-- ['HintMetricsDefault']  Hint metrics in the default manner for the font
--                         backend and target device
--
-- ['HintMetricsOff']      Do not hint font metrics
--
-- ['HintMetricsOn']       Hint font metrics
--
--
{#enum hint_metrics_t as HintMetrics {underscoreToCase} deriving(Eq,Show)#}

-- | Specifies how to render text.
{#pointer *font_options_t as FontOptions foreign newtype#}

withFontOptions (FontOptions fptr) = withForeignPtr fptr

mkFontOptions :: Ptr FontOptions -> IO FontOptions
mkFontOptions fontOptionsPtr = do
  fontOptionsForeignPtr <- newForeignPtr fontOptionsDestroy fontOptionsPtr
  return (FontOptions fontOptionsForeignPtr)

foreign import ccall unsafe "&cairo_font_options_destroy"
  fontOptionsDestroy :: FinalizerPtr FontOptions

-- XXX: pathToList :: Path -> [PathData]
--
-- http://cairographics.org/manual/bindings-path.html
--
-- {#enum path_data_type_t as PathDataType {underscoreToCase}#}
--
-- type Point = (Double, Double)
-- data PathData = PathMoveTo Point
--               | PathLineTo Point
--               | PathCurveTo Point Point Point
--               | PathClose

-- | A Cairo path.
--
-- * A path is a sequence of drawing operations that are accumulated until
--   'Graphics.Rendering.Cairo.stroke' is called. Using a path is particularly
--   useful when drawing lines with special join styles and
--   'Graphics.Rendering.Cairo.closePath'.
--
{#pointer *path_t as Path newtype#}
unPath (Path x) = x

#if CAIRO_CHECK_VERSION(1,10,0)

{#pointer *rectangle_int_t as RectangleIntPtr -> RectangleInt#}

-- | A data structure for holding a rectangle with integer coordinates.
data RectangleInt = RectangleInt {
    x      :: Int
  , y      :: Int
  , width  :: Int
  , height :: Int
  }

instance Storable RectangleInt where
  sizeOf _ = {#sizeof rectangle_int_t#}
  alignment _ = alignment (undefined :: CInt)
  peek p = do
    x      <- {#get rectangle_int_t->x#}      p
    y      <- {#get rectangle_int_t->y#}      p
    width  <- {#get rectangle_int_t->width#}  p
    height <- {#get rectangle_int_t->height#} p
    return $ RectangleInt (fromIntegral x) (fromIntegral y) (fromIntegral width) (fromIntegral height)
  poke p (RectangleInt {..}) = do
    {#set rectangle_int_t->x#}      p (fromIntegral x)
    {#set rectangle_int_t->y#}      p (fromIntegral y)
    {#set rectangle_int_t->width#}  p (fromIntegral width)
    {#set rectangle_int_t->height#} p (fromIntegral height)
    return ()

-- | Used as the return value for regionContainsRectangle.
{#enum cairo_region_overlap_t as RegionOverlap {underscoreToCase} deriving(Eq,Show)#}

-- | A Cairo region. Represents a set of integer-aligned rectangles.
--
-- It allows set-theoretical operations like regionUnion and regionIntersect to be performed on them.
{#pointer *region_t as Region foreign newtype#}

withRegion (Region fptr) = withForeignPtr fptr

mkRegion :: Ptr Region -> IO Region
mkRegion regionPtr = do
  regionForeignPtr <- newForeignPtr regionDestroy regionPtr
  return (Region regionForeignPtr)

foreign import ccall unsafe "&cairo_region_destroy"
  regionDestroy :: FinalizerPtr Region

#endif

{#enum content_t as Content {underscoreToCase} deriving(Eq,Show)#}

data Format = FormatARGB32
            | FormatRGB24
            | FormatA8
            | FormatA1
            deriving (Enum,Show,Eq)

-- | FIXME: We should find out about this.
{#enum extend_t as Extend {underscoreToCase} deriving(Eq,Show)#}

-- | Specify how filtering is done.
{#enum filter_t as Filter {underscoreToCase} deriving(Eq,Show)#}

-- Marshalling functions

{-# INLINE cIntConv #-}
cIntConv :: (Integral a, Integral b) => a -> b
cIntConv  = fromIntegral

{-# INLINE cFloatConv #-}
cFloatConv :: (RealFloat a, RealFloat b) => a -> b
cFloatConv  = realToFrac

{-# INLINE cFromBool #-}
cFromBool :: Num a => Bool -> a
cFromBool  = fromBool

{-# INLINE cToBool #-}
cToBool :: (Eq a, Num a) => a -> Bool
cToBool  = toBool

{-# INLINE cToEnum #-}
cToEnum :: (Integral i, Enum e) => i -> e
cToEnum  = toEnum . cIntConv

{-# INLINE cFromEnum #-}
cFromEnum :: (Enum e, Integral i) => e -> i
cFromEnum  = cIntConv . fromEnum

{-# INLINE peekFloatConv #-}
peekFloatConv :: (Storable a, RealFloat a, RealFloat b) =>  Ptr a -> IO b
peekFloatConv  = liftM cFloatConv . peek

{-# INLINE withFloatConv #-}
withFloatConv :: (Storable b, RealFloat a, RealFloat b) => a -> (Ptr b -> IO c) -> IO c
withFloatConv  = with . cFloatConv

{-# INLINE withArrayFloatConv #-}
withArrayFloatConv :: (Storable b, RealFloat a, RealFloat b) => [a] -> (Ptr b -> IO b1) -> IO b1
withArrayFloatConv = withArray . map (cFloatConv)
