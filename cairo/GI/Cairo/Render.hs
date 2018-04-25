{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, CPP #-}

-- The following is all rather brittle: We need to pre-process this file with GHC
-- in order to get the __GLASGOW_HASKELL__ macro (which we should replace with a
-- version test of the array package). At the same time we need to version of
-- Cairo and the macros for testing it. We sneakily get the version from the
-- internal cairo-version.h file but we have to define the testing macros ourselves.
#include<cairo-features.h>

-- GTK-2.12 doesn't have cairo-version.h, but defines the appropriate VERSION
-- variables in cairo-features.h instead. So only include this when necessary.
#ifndef CAIRO_VERSION_MAJOR
#include<cairo-version.h>
#endif
#define CAIRO_VERSION_ENCODE(major, minor, micro) (     \
          ((major) * 10000)                             \
        + ((minor) *   100)                             \
        + ((micro) *     1))

#define CAIRO_VERSION CAIRO_VERSION_ENCODE(     \
        CAIRO_VERSION_MAJOR,                    \
        CAIRO_VERSION_MINOR,                    \
        CAIRO_VERSION_MICRO)

#define CAIRO_CHECK_VERSION(major,minor,micro)    \
        (CAIRO_VERSION >= CAIRO_VERSION_ENCODE(major,minor,micro))
-----------------------------------------------------------------------------
-- |
-- Module      :  GI.Cairo.Render
-- Copyright   :  (c) Paolo Martini 2005, (c) Abraham Egnor 2004, (c) Aetion Technologies LLC 2004
-- License     :  BSD-style (see cairo/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- The Cairo 2D graphics library.
--
-- Cairo is a 2D graphics library with support for multiple output devices.
-- Currently supported output targets include the X Window System, win32, and
-- image buffers. Experimental backends include OpenGL (through glitz), Quartz,
-- XCB, PostScript and PDF file output.
--
-- Cairo is designed to produce consistent output on all output media while
-- taking advantage of display hardware acceleration when available (eg.
-- through the X Render Extension).
--
-- The cairo API provides operations similar to the drawing operators of
-- PostScript and PDF. Operations in cairo including stroking and filling cubic
-- Bezier splines, transforming and compositing translucent images, and
-- antialiased text rendering. All drawing operations can be transformed by any
-- affine transformation (scale, rotation, shear, etc.)
--
-- Cairo is free software and is available to be redistributed and\/or modified
-- under the terms of either the GNU Lesser General Public License (LGPL)
-- version 2.1 or the Mozilla Public License (MPL) version 1.1.
--
-- For more information see <http://cairographics.org>
--
-- * Note the Haskell bindings do not support all the possible cairo backends
-- because it would require bindings for the associated technology (eg X11,
-- glitz, etc) however bindings to other backends may be implemented
-- externally. For example, Gtk2Hs provides a binding to the backend for X11
-- (and win32 on Windows).
-----------------------------------------------------------------------------
module GI.Cairo.Render (
  -- * Drawing
    renderWith
  , save
  , restore
  , status
  , withTargetSurface
  , pushGroup
  , pushGroupWithContent
  , popGroupToSource
  , setSourceRGB
  , setSourceRGBA
  , setSource
  , setSourceSurface
  , getSource
  , setAntialias
  , setDash
  , setFillRule
  , getFillRule
  , setLineCap
  , getLineCap
  , setLineJoin
  , getLineJoin
  , setLineWidth
  , getLineWidth
  , setMiterLimit
  , getMiterLimit
  , setOperator
  , getOperator
  , setTolerance
  , getTolerance
  , clip
  , clipPreserve
  , clipExtents
  , resetClip
  , fill
  , fillPreserve
  , fillExtents
  , inFill
  , mask
  , maskSurface
  , paint
  , paintWithAlpha
  , stroke
  , strokePreserve
  , strokeExtents
  , inStroke
  , copyPage
  , showPage

  -- ** Paths
  , getCurrentPoint
  , newPath
  , closePath
  , arc
  , arcNegative
  , curveTo
  , lineTo
  , moveTo
  , rectangle
  , textPath
  , relCurveTo
  , relLineTo
  , relMoveTo

  -- ** Patterns
  , withRGBPattern
  , withRGBAPattern
  , withPatternForSurface
  , withGroupPattern
  , withLinearPattern
  , withRadialPattern
  , patternAddColorStopRGB
  , patternAddColorStopRGBA
  , patternSetMatrix
  , patternGetMatrix
  , patternSetExtend
  , patternGetExtend
  , patternSetFilter
  , patternGetFilter

  -- ** Transformations
  , translate
  , scale
  , rotate
  , transform
  , setMatrix
  , getMatrix
  , identityMatrix
  , userToDevice
  , userToDeviceDistance
  , deviceToUser
  , deviceToUserDistance

  -- ** Text
  , selectFontFace
  , setFontSize
  , setFontMatrix
  , getFontMatrix
  , setFontOptions
  , showText
  , fontExtents
  , textExtents

  -- * Fonts

  -- ** Font options
  , fontOptionsCreate
  , fontOptionsCopy
  , fontOptionsMerge
  , fontOptionsHash
  , fontOptionsEqual
  , fontOptionsSetAntialias
  , fontOptionsGetAntialias
  , fontOptionsSetSubpixelOrder
  , fontOptionsGetSubpixelOrder
  , fontOptionsSetHintStyle
  , fontOptionsGetHintStyle
  , fontOptionsSetHintMetrics
  , fontOptionsGetHintMetrics

  -- * Surfaces

  , withSimilarSurface
  , createSimilarSurface
  , renderWithSimilarSurface
  , surfaceGetFontOptions
  , surfaceFinish
  , surfaceFlush
  , surfaceMarkDirty
  , surfaceMarkDirtyRectangle
  , surfaceSetDeviceOffset

  -- ** Image surfaces
  , withImageSurface
  , withImageSurfaceForData
#if CAIRO_CHECK_VERSION(1,6,0)
  , formatStrideForWidth
#endif
  , createImageSurfaceForData
  , createImageSurface
  , imageSurfaceGetWidth
  , imageSurfaceGetHeight
#if CAIRO_CHECK_VERSION(1,2,0)
  , imageSurfaceGetFormat
  , imageSurfaceGetStride
#if  __GLASGOW_HASKELL__ >= 606
  , imageSurfaceGetData
#endif
  , SurfaceData
  , imageSurfaceGetPixels
#endif

#ifdef CAIRO_HAS_PNG_FUNCTIONS
  -- ** PNG support
  , withImageSurfaceFromPNG
  , imageSurfaceCreateFromPNG
  , surfaceWriteToPNG
#endif

#ifdef CAIRO_HAS_PDF_SURFACE
  -- ** PDF surfaces
  , withPDFSurface
#if CAIRO_CHECK_VERSION(1,2,0)
  , pdfSurfaceSetSize
#endif
#endif

#ifdef CAIRO_HAS_PS_SURFACE
  -- ** PS surfaces
  , withPSSurface
#if CAIRO_CHECK_VERSION(1,2,0)
  , psSurfaceSetSize
#endif
#endif

#ifdef CAIRO_HAS_SVG_SURFACE
  -- ** SVG surfaces
  , withSVGSurface
#endif

#if CAIRO_CHECK_VERSION(1,10,0)
  -- * Regions
  , regionCreate
  , regionCreateRectangle
  , regionCreateRectangles
  , regionCopy
  , regionGetExtents
  , regionNumRectangles
  , regionGetRectangle
  , regionIsEmpty
  , regionContainsPoint
  , regionContainsRectangle
  , regionEqual
  , regionTranslate
  , regionIntersect
  , regionIntersectRectangle
  , regionSubtract
  , regionSubtractRectangle
  , regionUnion
  , regionUnionRectangle
  , regionXor
  , regionXorRectangle

#endif

  -- * Utilities

  , liftIO
  , version
  , versionString
  , CairoString

  -- * Types

  , Render
  , Matrix
  , Surface
  , Pattern
  , Status(..)
  , Operator(..)
  , Antialias(..)
  , FillRule(..)
  , LineCap(..)
  , LineJoin(..)
  , ScaledFont
  , FontFace
  , Glyph
  , TextExtents(..)
  , FontExtents(..)
  , FontSlant(..)
  , FontWeight(..)
  , SubpixelOrder(..)
  , HintStyle(..)
  , HintMetrics(..)
  , FontOptions
  , Path
#if CAIRO_CHECK_VERSION(1,10,0)
  , RectangleInt(..)
  , RegionOverlap(..)
  , Region
#endif
  , Content(..)
  , Format(..)
  , Extend(..)
  , Filter(..)

  ) where

import Data.GI.Base (wrapBoxed)
import Control.Monad (unless, when)
import Control.Monad.Reader (ReaderT(runReaderT), ask, MonadIO, liftIO)
import Control.Exception (bracket)
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.Storable (Storable(..))
import Foreign.ForeignPtr ( touchForeignPtr )
#if __GLASGOW_HASKELL__ >= 606
import qualified Data.ByteString as BS
#endif
import Data.Ix
-- internal module of GHC
import Data.Array.Base ( MArray, newArray, newArray_, unsafeRead, unsafeWrite,
#if __GLASGOW_HASKELL__ < 605
                         HasBounds, bounds
#else
                         getBounds
#endif
#if __GLASGOW_HASKELL__ >= 608
                         ,getNumElements
#endif
                       )
#ifdef CAIRO_HAS_PNG_FUNCTIONS
import GI.Cairo.Render.Internal (imageSurfaceCreateFromPNG)
#endif

import GI.Cairo.Render.Types
import GI.Cairo.Render.Internal.Utilities (CairoString(..))
import qualified GI.Cairo.Render.Internal as Internal
import GI.Cairo.Render.Internal (Render(..), bracketR)

liftRender0 :: (Cairo -> IO a) -> Render a
liftRender0 f = ask >>= \context -> liftIO (f context)
liftRender1 :: (Cairo -> a -> IO b) -> a -> Render b
liftRender1 f a = ask >>= \context -> liftIO (f context a)
liftRender2 :: (Cairo -> a -> b -> IO c) -> a -> b -> Render c
liftRender2 f a b = ask >>= \context -> liftIO (f context a b)
liftRender3 :: (Cairo -> a -> b -> c -> IO d) -> a -> b -> c -> Render d
liftRender3 f a b c = ask >>= \context -> liftIO (f context a b c)
liftRender4 :: (Cairo -> a -> b -> c -> d -> IO e) -> a -> b -> c -> d -> Render e
liftRender4 f a b c d = ask >>= \context -> liftIO (f context a b c d)
liftRender5 :: (Cairo -> a -> b -> c -> d -> e -> IO f) -> a -> b -> c -> d -> e -> Render f
liftRender5 f a b c d e = ask >>= \context -> liftIO (f context a b c d e)
liftRender6 :: (Cairo -> a -> b -> c -> d -> e -> f -> IO g) -> a -> b -> c -> d -> e -> f -> Render g
liftRender6 f a b c d e g = ask >>= \context -> liftIO (f context a b c d e g)

-- | Creates a new Render context with all graphics state parameters set to
-- default values and with the given surface as a target surface. The target
-- surface should be constructed with a backend-specific function such as
-- 'withImageSurface' (or any other with\<backend\>Surface variant).
--
renderWith :: (MonadIO m) =>
     Surface  -- ^ the target surface for the Render context
  -> Render a
  -> m a
renderWith surface (Render m) = liftIO $
  bracket (do context <- Internal.create surface
              wrapBoxed Cairo context)
          (\context -> do status <- Internal.status context
                          unless (status == StatusSuccess) $
                            fail =<< Internal.statusToString status)
          (\context -> runReaderT m context)

-- | Makes a copy of the current state and saves it on an internal stack of
-- saved states. When 'restore' is called, the saved state is restored.
-- Multiple calls to 'save' and 'restore' can be nested; each call to 'restore'
-- restores the state from the matching paired 'save'.
--
save :: Render ()
save = liftRender0 Internal.save

-- | Restores to the state saved by a preceding call to 'save' and removes that
-- state from the stack of saved states.
--
restore :: Render ()
restore = liftRender0 Internal.restore


-- | Ask for the status of the current 'Render' monad.
--
status :: Render Status
status = liftRender0 Internal.status

-- | Gets the target surface for the Render context as passed to 'renderWith'.
--
withTargetSurface :: (Surface -> Render a) -> Render a
withTargetSurface f = do
  context <- ask
  surface <- liftIO $ Internal.getTarget context
  f surface

-- | Like @pushGroupWithContent ContentColorAlpha@, but more convenient.
pushGroup :: Render ()
pushGroup = liftRender0 Internal.pushGroup

-- | Temporarily redirects drawing to an intermediate surface known as a group.
-- The redirection lasts until the group is completed by a call to
-- 'withGroupPattern' or 'popGroupToSource'. These calls provide the result of
-- any drawing to the group as a pattern (either as an explicit object, or set
-- as the source pattern).  This group functionality can be convenient for
-- performing intermediate compositing. One common use of a group is to render
-- objects as opaque within the group (so that they occlude each other), and
-- then blend the result with translucence onto the destination.
--
-- Groups can be nested arbitrarily deeply by making balanced calls to
-- 'pushGroupWithContent' and 'withGroupPattern'. As a side effect,
-- 'pushGroupWithContent' calls 'save' and 'withGroupPattern' calls 'restore',
-- so that any changes to the graphics state will not be visible outside the
-- group.
--
-- As an example, here is how one might fill and stroke a path with
-- translucence, but without any portion of the fill being visible under the
-- stroke:
--
-- > pushGroup
-- > setSource fillPattern
-- > fillPreserve
-- > setSource strokePattern
-- > stroke
-- > popGroupToSource
-- > paintWithAlpha alpha
pushGroupWithContent :: Content -> Render ()
pushGroupWithContent = liftRender1 Internal.pushGroupWithContent

-- | Like @withGroupPattern setSource@, but more convenient.
popGroupToSource :: Render ()
popGroupToSource = liftRender0 Internal.popGroupToSource

-- | Sets the source pattern within the context to an opaque color. This opaque
-- color will then be used for any subsequent drawing operation until a new source
-- pattern is set.
--
-- The color components are floating point numbers in the range 0 to 1. If the
-- values passed in are outside that range, they will be clamped.
--
setSourceRGB ::
     Double -- ^ red component of colour
  -> Double -- ^ green component of colour
  -> Double -- ^ blue compoment of colour
  -> Render ()
setSourceRGB = liftRender3 Internal.setSourceRGB

-- | Sets the source pattern within the context to a translucent color. This
-- color will then be used for any subsequent drawing operation until a new
-- source pattern is set.
--
-- The color and alpha components are floating point numbers in the range 0 to
-- 1. If the values passed in are outside that range, they will be clamped.
--
setSourceRGBA ::
     Double -- ^ red component of color
  -> Double -- ^ green component of color
  -> Double -- ^ blue component of color
  -> Double -- ^ alpha component of color
  -> Render ()
setSourceRGBA = liftRender4 Internal.setSourceRGBA

-- | Sets the source pattern within the context to source. This pattern will
-- then be used for any subsequent drawing operation until a new source pattern
-- is set.
--
-- Note: The pattern's transformation matrix will be locked to the user space
-- in effect at the time of 'setSource'. This means that further
-- modifications of the current transformation matrix will not affect the source
-- pattern. See 'setMatrix'.
--
setSource ::
     Pattern -- ^ a 'Pattern' to be used as the source for subsequent drawing
             -- operations.
  -> Render ()
setSource = liftRender1 Internal.setSource

-- | This is a convenience function for creating a pattern from surface and
-- setting it as the source in the context with 'setSource'.
--
-- The x and y parameters give the user-space coordinate at which the surface
-- origin should appear. (The surface origin is its upper-left corner before any
-- transformation has been applied.) The x and y patterns are negated and then
-- set as translation values in the pattern matrix.
--
-- Other than the initial translation pattern matrix, as described above, all
-- other pattern attributes, (such as its extend mode), are set to the default
-- values as in 'patternCreateForSurface'. The resulting pattern can be queried
-- with 'getSource' so that these attributes can be modified if desired, (eg. to
-- create a repeating pattern with 'patternSetExtent'.
--
setSourceSurface ::
     Surface -- ^ a surface to be used to set the source pattern
  -> Double  -- ^ user-space X coordinate for surface origin
  -> Double  -- ^ user-space Y coordinate for surface origin
  -> Render ()
setSourceSurface = liftRender3 Internal.setSourceSurface

-- | Gets the current source pattern.
--
getSource :: Render Pattern
getSource = liftRender0 Internal.getSource

-- | Set the antialiasing mode of the rasterizer used for drawing shapes. This
-- value is a hint, and a particular backend may or may not support a particular
-- value. At the current time, no backend supports 'AntialiasSubpixel' when
-- drawing shapes.
--
-- Note that this option does not affect text rendering, instead see
-- 'fontOptionsSetAntilias'.
--
setAntialias ::
     Antialias -- ^ the new antialiasing mode
  -> Render ()
setAntialias = liftRender1 Internal.setAntialias

-- | Gets the current shape antialiasing mode, as set by 'setAntialias'.
--
getAntialias :: Render Antialias
getAntialias = liftRender0 Internal.getAntialias

-- | Sets the dash pattern to be used by 'stroke'. A dash pattern is specified
-- by dashes, a list of positive values. Each value provides the user-space
-- length of altenate "on" and "off" portions of the stroke. The offset
-- specifies an offset into the pattern at which the stroke begins.
--
-- If @dashes@ is @[]@ then dashing is disabled.

-- If @dashes@ is @[a]@ a symmetric pattern is assumed with alternating on and
-- off portions of the size specified by the single value in dashes.

-- If any value in @dashes@ is negative, or if all values are 0, then context
-- will be put into an error state with a status of 'StatusInvalidDash'.
--
setDash ::
     [Double] -- ^ @dashes@ a list specifying alternate lengths of on and off
              -- portions of the stroke
  -> Double   -- ^ an offset into the dash pattern at which the stroke should
              -- start
  -> Render ()
setDash = liftRender2 Internal.setDash

-- | Set the current fill rule within the cairo context. The fill rule is used
-- to determine which regions are inside or outside a complex (potentially
-- self-intersecting) path. The current fill rule affects both 'fill' and
-- 'clip'. See 'FillRule' for details on the semantics of each available fill
-- rule.
--
setFillRule ::
     FillRule -- ^ a fill rule
  -> Render ()
setFillRule = liftRender1 Internal.setFillRule

-- | Gets the current fill rule, as set by 'setFillrule'.
--
getFillRule :: Render FillRule
getFillRule = liftRender0 Internal.getFillRule

-- | Sets the current line cap style within the cairo context. See 'LineCap'
-- for details about how the available line cap styles are drawn.
--
-- As with the other stroke parameters, the current line cap style is examined
-- by 'stroke', 'strokeExtents', and 'strokeToPath', but does not have any
-- effect during path construction.
--
setLineCap ::
     LineCap -- ^ a line cap style
  -> Render ()
setLineCap = liftRender1 Internal.setLineCap

-- | Gets the current line cap style, as set by 'setLineCap'.
--
getLineCap :: Render LineCap
getLineCap = liftRender0 Internal.getLineCap

-- | Sets the current line join style within the cairo context. See 'LineJoin'
-- for details about how the available line join styles are drawn.
--
-- As with the other stroke parameters, the current line join style is examined
-- by 'stroke', 'strokeExtents', and 'strokeToPath', but does not have any
-- effect during path construction.
--
setLineJoin ::
     LineJoin -- ^ a line joint style
  -> Render ()
setLineJoin = liftRender1 Internal.setLineJoin

-- | Gets the current line join style, as set by 'setLineJoin'.
--
getLineJoin :: Render LineJoin
getLineJoin = liftRender0 Internal.getLineJoin

-- | Sets the current line width within the cairo context. The line width
-- specifies the diameter of a pen that is circular in user-space.
--
-- As with the other stroke parameters, the current line cap style is examined
-- by 'stroke', 'strokeExtents', and 'strokeToPath', but does not have any
-- effect during path construction.
--
setLineWidth ::
     Double -- ^ a line width
  -> Render ()
setLineWidth = liftRender1 Internal.setLineWidth

-- | Gets the current line width, as set by 'setLineWidth'.
--
getLineWidth :: Render Double
getLineWidth = liftRender0 Internal.getLineWidth

-- |
--
setMiterLimit ::
     Double -- ^ -
  -> Render ()
setMiterLimit = liftRender1 Internal.setMiterLimit

-- | Gets the current miter limit, as set by 'setMiterLimit'.
--
getMiterLimit :: Render Double
getMiterLimit = liftRender0 Internal.getMiterLimit

-- | Sets the compositing operator to be used for all drawing operations.
-- See 'Operator' for details on the semantics of each available compositing
-- operator.
--
setOperator ::
     Operator -- ^ a compositing operator
  -> Render ()
setOperator = liftRender1 Internal.setOperator

-- | Gets the current compositing operator for a cairo context.
--
getOperator :: Render Operator
getOperator = liftRender0 Internal.getOperator

-- | Sets the tolerance used when converting paths into trapezoids. Curved
-- segments of the path will be subdivided until the maximum deviation between
-- the original path and the polygonal approximation is less than tolerance.
-- The default value is 0.1. A larger value will give better performance,
-- a smaller value, better appearance. (Reducing the value from the default
-- value of 0.1 is unlikely to improve appearance significantly.)
--
setTolerance ::
     Double -- ^ the tolerance, in device units (typically pixels)
  -> Render ()
setTolerance = liftRender1 Internal.setTolerance

-- | Gets the current tolerance value, as set by 'setTolerance'.
--
getTolerance :: Render Double
getTolerance = liftRender0 Internal.getTolerance

-- | Establishes a new clip region by intersecting the current clip region with
-- the current path as it would be filled by 'fill' and according to the current
-- fill rule (see 'setFillRule').
--
-- After 'clip', the current path will be cleared from the cairo context.
--
-- The current clip region affects all drawing operations by effectively masking
-- out any changes to the surface that are outside the current clip region.
--
-- Calling 'clip' can only make the clip region smaller, never larger. But the
-- current clip is part of the graphics state, so a temporary restriction of the
-- clip region can be achieved by calling 'clip' within a 'save'/'restore' pair.
-- The only other means of increasing the size of the clip region is 'resetClip'.
--
clip :: Render ()
clip = liftRender0 Internal.clip

-- | Establishes a new clip region by intersecting the current clip region with
-- the current path as it would be filled by 'fill' and according to the current
-- fill rule (see 'setFillRule').
--
-- Unlike 'clip', cairoClipPreserve preserves the path within the cairo context.
--
-- The current clip region affects all drawing operations by effectively masking
-- out any changes to the surface that are outside the current clip region.
--
-- Calling 'clip' can only make the clip region smaller, never larger. But the
-- current clip is part of the graphics state, so a temporary restriction of the
-- clip region can be achieved by calling 'clip' within a 'save'/'restore' pair.
-- The only other means of increasing the size of the clip region is 'resetClip'.
--
clipPreserve :: Render ()
clipPreserve = liftRender0 Internal.clipPreserve

-- | Reset the current clip region to its original, unrestricted state. That is,
-- set the clip region to an infinitely large shape containing the target
-- surface. Equivalently, if infinity is too hard to grasp, one can imagine the
-- clip region being reset to the exact bounds of the target surface.
--
-- Note that code meant to be reusable should not call 'resetClip' as it will
-- cause results unexpected by higher-level code which calls 'clip'. Consider
-- using 'save' and 'restore' around 'clip' as a more robust means of
-- temporarily restricting the clip region.
--
resetClip :: Render ()
resetClip = liftRender0 Internal.resetClip

-- | Computes a bounding box in user coordinates covering the area
-- inside the current clip.
--
clipExtents :: Render (Double,Double,Double,Double)
clipExtents = liftRender0 Internal.clipExtents

-- | A drawing operator that fills the current path according to the current
-- fill rule, (each sub-path is implicitly closed before being filled).
-- After 'fill', the current path will be cleared from the cairo context.
--
-- See 'setFillRule' and 'fillPreserve'.
--
fill :: Render ()
fill = liftRender0 Internal.fill

-- | A drawing operator that fills the current path according to the current
-- fill rule, (each sub-path is implicitly closed before being filled).
-- Unlike 'fill', 'fillPreserve' preserves the path within the cairo context.
--
-- See 'setFillRule' and 'fill'.
--
fillPreserve :: Render ()
fillPreserve = liftRender0 Internal.fillPreserve

-- |
--
fillExtents :: Render (Double,Double,Double,Double)
fillExtents = liftRender0 Internal.fillExtents

-- |
--
inFill :: Double -> Double -> Render Bool
inFill = liftRender2 Internal.inFill

-- | A drawing operator that paints the current source using the alpha channel
-- of pattern as a mask. (Opaque areas of mask are painted with the source,
-- transparent areas are not painted.)
--
mask ::
     Pattern -- ^ a 'Pattern'
  -> Render ()
mask = liftRender1 Internal.mask

-- | A drawing operator that paints the current source using the alpha channel
-- of surface as a mask. (Opaque areas of surface are painted with the source,
-- transparent areas are not painted.)
--
maskSurface ::
     Surface -- ^ a 'Surface'
  -> Double  -- ^ X coordinate at which to place the origin of surface
  -> Double  -- ^ Y coordinate at which to place the origin of surface
  -> Render ()
maskSurface = liftRender3 Internal.maskSurface

-- | A drawing operator that paints the current source everywhere within the
-- current clip region.
--
paint :: Render ()
paint = liftRender0 Internal.paint

-- | A drawing operator that paints the current source everywhere within the
-- current clip region using a mask of constant alpha value alpha. The effect
-- is similar to 'paint', but the drawing is faded out using the alpha value.
--
paintWithAlpha ::
     Double -- ^ alpha value, between 0 (transparent) and 1 (opaque)
  -> Render ()
paintWithAlpha = liftRender1 Internal.paintWithAlpha

-- | A drawing operator that strokes the current path according to the current
-- line width, line join, line cap, and dash settings. After issuing 'stroke',
-- the current path will be cleared from the 'Render' monad.
--
-- See 'setLineWidth', 'setLineJoin', 'setLineCap', 'setDash', and 'strokePreserve'.
--
stroke :: Render ()
stroke = liftRender0 Internal.stroke

-- | A drawing operator that strokes the current path according to the current
-- line width, line join, line cap, and dash settings. Unlike 'stroke',
-- 'strokePreserve' preserves the path within the 'Render' monad.
--
-- See 'setLineWidth', 'setLineJoin', 'setLineCap', 'setDash', and 'strokePreserve'.
--
strokePreserve :: Render ()
strokePreserve = liftRender0 Internal.strokePreserve

-- |
--
strokeExtents :: Render (Double,Double,Double,Double)
strokeExtents = liftRender0 Internal.strokeExtents

-- |
--
inStroke :: Double -> Double -> Render Bool
inStroke = liftRender2 Internal.inStroke

-- |
--
copyPage :: Render ()
copyPage = liftRender0 Internal.copyPage

-- |
--
showPage :: Render ()
showPage = liftRender0 Internal.showPage


-- | Gets the current point of the current path, which is conceptually the final
-- point reached by the path so far.
--
-- The current point is returned in the user-space coordinate system. If there
-- is no defined current point then x and y will both be set to 0.0.
--
-- Most path construction functions alter the current point. See the following
-- for details on how they affect the current point: 'newPath', 'moveTo',
-- 'lineTo', 'curveTo', 'arc', 'relMoveTo', 'relLineTo', 'relCurveTo',
-- 'arcNegative', 'textPath', 'strokeToPath'.
--
getCurrentPoint :: Render (Double,Double)
getCurrentPoint = liftRender0 Internal.getCurrentPoint

-- | Clears the current path. After this call there will be no current point.
--
newPath :: Render ()
newPath = liftRender0 Internal.newPath

-- | Adds a line segment to the path from the current point to the beginning of
-- the current subpath, (the most recent point passed to 'moveTo'), and closes
-- this subpath.
--
-- The behavior of 'closePath' is distinct from simply calling 'lineTo' with the
-- equivalent coordinate in the case of stroking. When a closed subpath is
-- stroked, there are no caps on the ends of the subpath. Instead, their is a
-- line join connecting the final and initial segments of the subpath.
--
closePath :: Render ()
closePath = liftRender0 Internal.closePath

-- | Adds a circular arc of the given radius to the current path. The arc is
-- centered at (@xc@, @yc@), begins at @angle1@ and proceeds in the direction of
-- increasing angles to end at @angle2@. If @angle2@ is less than @angle1@ it
-- will be progressively increased by @2*pi@ until it is greater than @angle1@.
--
-- If there is a current point, an initial line segment will be added to the
-- path to connect the current point to the beginning of the arc.
--
-- Angles are measured in radians. An angle of 0 is in the direction of the
-- positive X axis (in user-space). An angle of @pi/2@ radians (90 degrees) is in
-- the direction of the positive Y axis (in user-space). Angles increase in the
-- direction from the positive X axis toward the positive Y axis. So with the
-- default transformation matrix, angles increase in a clockwise direction.
--
-- (To convert from degrees to radians, use @degrees * (pi \/ 180)@.)
--
-- This function gives the arc in the direction of increasing angles; see
-- 'arcNegative' to get the arc in the direction of decreasing angles.
--
-- The arc is circular in user-space. To achieve an elliptical arc, you can
-- scale the current transformation matrix by different amounts in the X and Y
-- directions. For example, to draw an ellipse in the box given by x, y, width,
-- height:
--
-- > save
-- > translate (x + width / 2) (y + height / 2)
-- > scale (1 / (height / 2.)) (1 / (width / 2))
-- > arc 0 0 1 0 (2 * pi)
-- > restore
--
arc ::
     Double -- ^ @xc@ - X position of the center of the arc
  -> Double -- ^ @yc@ - Y position of the center of the arc
  -> Double -- ^ @radius@ - the radius of the arc
  -> Double -- ^ @angle1@ - the start angle, in radians
  -> Double -- ^ @angle2@ - the end angle, in radians
  -> Render ()
arc = liftRender5 Internal.arc

-- | Adds a circular arc of the given radius to the current path. The arc is
-- centered at (@xc@, @yc@), begins at @angle1@ and proceeds in the direction of
-- decreasing angles to end at @angle2@. If @angle2@ is greater than @angle1@ it
-- will be progressively decreased by 2*@pi@ until it is greater than @angle1@.
--
-- See 'arc' for more details. This function differs only in the direction of
-- the arc between the two angles.
--
arcNegative ::
     Double -- ^ @xc@ - X position of the center of the arc
  -> Double -- ^ @yc@ - Y position of the center of the arc
  -> Double -- ^ @radius@ - the radius of the arc
  -> Double -- ^ @angle1@ - the start angle, in radians
  -> Double -- ^ @angle2@ - the end angle, in radians
  -> Render ()
arcNegative = liftRender5 Internal.arcNegative

-- | Adds a cubic Bezier spline to the path from the current point to position
-- (@x3@, @y3@) in user-space coordinates, using (@x1@, @y1@) and (@x2@, @y2@)
-- as the control points. After this call the current point will be (@x3@, @y3@).
--
curveTo ::
     Double -- ^ @x1@ - the X coordinate of the first control point
  -> Double -- ^ @y1@ - the Y coordinate of the first control point
  -> Double -- ^ @x2@ - the X coordinate of the second control point
  -> Double -- ^ @y2@ - the Y coordinate of the second control point
  -> Double -- ^ @x3@ - the X coordinate of the end of the curve
  -> Double -- ^ @y3@ - the Y coordinate of the end of the curve
  -> Render ()
curveTo = liftRender6 Internal.curveTo

-- | Adds a line to the path from the current point to position (@x@, @y@) in
-- user-space coordinates. After this call the current point will be (@x@, @y@).
--
lineTo ::
     Double -- ^ @x@ - the X coordinate of the end of the new line
  -> Double -- ^ @y@ - the Y coordinate of the end of the new line
  -> Render ()
lineTo = liftRender2 Internal.lineTo

-- | If the current subpath is not empty, begin a new subpath. After this call
-- the current point will be (@x@, @y@).
--
moveTo ::
     Double -- ^ @x@ - the X coordinate of the new position
  -> Double -- ^ @y@ - the Y coordinate of the new position
  -> Render ()
moveTo = liftRender2 Internal.moveTo

-- | Adds a closed-subpath rectangle of the given size to the current path at
-- position (@x@, @y@) in user-space coordinates.
--
rectangle ::
     Double -- ^ @x@ - the X coordinate of the top left corner of the rectangle
  -> Double -- ^ @y@ - the Y coordinate of the top left corner of the rectangle
  -> Double -- ^ @width@ - the width of the rectangle
  -> Double -- ^ @height@ - the height of the rectangle
  -> Render ()
rectangle = liftRender4 Internal.rectangle

-- | Render text at the current path.
--
-- * See 'showText' for why you should use Gtk functions.
--
textPath ::
     CairoString string
  => string -- ^ -
  -> Render ()
textPath = liftRender1 Internal.textPath

-- | Relative-coordinate version of 'curveTo'. All offsets are relative to the
-- current point. Adds a cubic Bezier spline to the path from the current point
-- to a point offset from the current point by (@dx3@, @dy3@), using points
-- offset by (@dx1@, @dy1@) and (@dx2@, @dy2@) as the control points. After this
-- call the current point will be offset by (@dx3@, @dy3@).
--
-- Given a current point of (x, y), relCurveTo @dx1@ @dy1@ @dx2@ @dy2@ @dx3@ @dy3@
-- is logically equivalent to curveTo (x + @dx1@) (y + @dy1@) (x + @dx2@) (y + @dy2@) (x + @dx3@) (y + @dy3@).
--
relCurveTo ::
     Double -- ^ @dx1@ - the X offset to the first control point
  -> Double -- ^ @dy1@ - the Y offset to the first control point
  -> Double -- ^ @dx2@ - the X offset to the second control point
  -> Double -- ^ @dy2@ - the Y offset to the second control point
  -> Double -- ^ @dx3@ - the X offset to the end of the curve
  -> Double -- ^ @dy3@ - the Y offset to the end of the curve
  -> Render ()
relCurveTo = liftRender6 Internal.relCurveTo

-- | Relative-coordinate version of 'lineTo'. Adds a line to the path from the
-- current point to a point that is offset from the current point by (@dx@, @dy@)
-- in user space. After this call the current point will be offset by (@dx@, @dy@).
--
-- Given a current point of (x, y), relLineTo @dx@ @dy@ is logically equivalent
-- to lineTo (x + @dx@) (y + @dy@).
--
relLineTo ::
     Double -- ^ @dx@ - the X offset to the end of the new line
  -> Double -- ^ @dy@ - the Y offset to the end of the new line
  -> Render ()
relLineTo = liftRender2 Internal.relLineTo

-- | If the current subpath is not empty, begin a new subpath. After this call
-- the current point will offset by (x, y).
--
-- Given a current point of (x, y), relMoveTo @dx@ @dy@ is logically equivalent
-- to moveTo (x + @dx@) (y + @dy@)
--
relMoveTo ::
     Double -- ^ @dx@ - the X offset
  -> Double -- ^ @dy@ - the Y offset
  -> Render ()
relMoveTo = liftRender2 Internal.relMoveTo


-- | Creates a new 'Pattern' corresponding to an opaque color. The color
-- components are floating point numbers in the range 0 to 1. If the values
-- passed in are outside that range, they will be clamped.
--
-- For example to create a solid red pattern:
--
-- > withRBGPattern 1 0 0 $ do
-- >   ...
-- >   ...
--
withRGBPattern ::
     Double -- ^ red component of the color
  -> Double -- ^ green component of the color
  -> Double -- ^ blue component of the color
  -> (Pattern -> Render a) -- ^ a nested render action using the pattern
  -> Render a
withRGBPattern r g b f =
  bracketR (Internal.patternCreateRGB r g b)
           (\pattern -> do status <- Internal.patternStatus pattern
                           liftIO $ Internal.patternDestroy pattern
                           unless (status == StatusSuccess) $
                             fail =<< Internal.statusToString status)
           (\pattern -> f pattern)

-- | Creates a new 'Pattern' corresponding to a translucent color. The color
-- components are floating point numbers in the range 0 to 1. If the values
-- passed in are outside that range, they will be clamped.
--
-- For example to create a solid red pattern at 50% transparency:
--
-- > withRBGPattern 1 0 0 0.5 $ do
-- >   ...
-- >   ...
--
withRGBAPattern ::
     Double -- ^ red component of color
  -> Double -- ^ green component of color
  -> Double -- ^ blue component of color
  -> Double -- ^ alpha component of color
  -> (Pattern -> Render a) -- ^ a nested render action using the pattern
  -> Render a
withRGBAPattern r g b a f =
  bracketR (Internal.patternCreateRGBA r g b a)
           (\pattern -> do status <- Internal.patternStatus pattern
                           liftIO $ Internal.patternDestroy pattern
                           unless (status == StatusSuccess) $
                             fail =<< Internal.statusToString status)
           (\pattern -> f pattern)

-- | Create a new 'Pattern' for the given surface.
--
withPatternForSurface ::
     Surface
  -> (Pattern -> Render a) -- ^ a nested render action using the pattern
  -> Render a
withPatternForSurface surface f =
  bracketR (Internal.patternCreateForSurface surface)
           (\pattern -> do status <- Internal.patternStatus pattern
                           liftIO $ Internal.patternDestroy pattern
                           unless (status == StatusSuccess) $
                             fail =<< Internal.statusToString status)
           (\pattern -> f pattern)

-- | Pop the current group from the group stack and use it as a pattern. The
-- group should be populated first by calling 'pushGroup' or
-- 'pushGroupWithContent' and doing some drawing operations. This also calls
-- 'restore' to balance the 'save' called in 'pushGroup'.
withGroupPattern :: (Pattern -> Render a) -- ^ a nested render action using the pattern
  -> Render a
withGroupPattern f = do
  context <- ask
  bracketR (Internal.popGroup context)
           (\pattern -> do status <- Internal.patternStatus pattern
                           liftIO $ Internal.patternDestroy pattern
                           unless (status == StatusSuccess) $
                             fail =<< Internal.statusToString status)
           f

-- | Create a new linear gradient 'Pattern' along the line defined by @(x0, y0)@
-- and @(x1, y1)@. Before using the gradient pattern, a number of color stops
-- should be defined using 'patternAddColorStopRGB' and 'patternAddColorStopRGBA'.
--
-- * Note: The coordinates here are in pattern space. For a new pattern,
-- pattern space is identical to user space, but the relationship between the
-- spaces can be changed with 'patternSetMatrix'.
--
withLinearPattern ::
     Double -- ^ @x0@ - x coordinate of the start point
  -> Double -- ^ @y0@ - y coordinate of the start point
  -> Double -- ^ @x1@ - x coordinate of the end point
  -> Double -- ^ @y1@ - y coordinate of the end point
  -> (Pattern -> Render a) -- ^ a nested render action using the pattern
  -> Render a
withLinearPattern x0 y0 x1 y1 f =
  bracketR (Internal.patternCreateLinear x0 y0 x1 y1)
           (\pattern -> do status <- Internal.patternStatus pattern
                           liftIO $ Internal.patternDestroy pattern
                           unless (status == StatusSuccess) $
                             fail =<< Internal.statusToString status)
           (\pattern -> f pattern)

-- | Creates a new radial gradient 'Pattern' between the two circles defined by
-- @(x0, y0, c0)@ and @(x1, y1, c0)@. Before using the gradient pattern, a
-- number of color stops should be defined using 'patternAddColorStopRGB'
-- or 'patternAddColorStopRGBA'.
--
-- * Note: The coordinates here are in pattern space. For a new pattern,
-- pattern space is identical to user space, but the relationship between the
-- spaces can be changed with 'patternSetMatrix'.
--
withRadialPattern ::
     Double -- ^ @cx0@ - x coordinate for the center of the start circle
  -> Double -- ^ @cy0@ - y coordinate for the center of the start circle
  -> Double -- ^ @radius0@ - radius of the start cirle
  -> Double -- ^ @cx1@ - x coordinate for the center of the end circle
  -> Double -- ^ @cy1@ - y coordinate for the center of the end circle
  -> Double -- ^ @radius1@ - radius of the end circle
  -> (Pattern -> Render a) -- ^ a nested render action using the pattern
  -> Render a
withRadialPattern cx0 cy0 radius0 cx1 cy1 radius1 f =
  bracketR (Internal.patternCreateRadial cx0 cy0 radius0 cx1 cy1 radius1)
           (\pattern -> do status <- Internal.patternStatus pattern
                           liftIO $ Internal.patternDestroy pattern
                           unless (status == StatusSuccess) $
                             fail =<< Internal.statusToString status)
           (\pattern -> f pattern)

-- | Adds an opaque color stop to a gradient pattern. The offset specifies the
-- location along the gradient's control vector. For example, a linear gradient's
-- control vector is from (x0,y0) to (x1,y1) while a radial gradient's control
-- vector is from any point on the start circle to the corresponding point on
-- the end circle.
--
-- The color is specified in the same way as in 'setSourceRGB'.
--
-- Note: If the pattern is not a gradient pattern, (eg. a linear or radial
-- pattern), then the pattern will be put into an error status with a status of
-- 'StatusPatternTypeMismatch'.
--
patternAddColorStopRGB ::
     MonadIO m =>
     Pattern -- ^ a 'Pattern'
  -> Double  -- ^ an offset in the range [0.0 .. 1.0]
  -> Double  -- ^ red component of color
  -> Double  -- ^ green component of color
  -> Double  -- ^ blue component of color
  -> m ()
patternAddColorStopRGB p offset r g b = liftIO $ Internal.patternAddColorStopRGB p offset r g b

-- | Adds a translucent color stop to a gradient pattern. The offset specifies
-- the location along the gradient's control vector. For example, a linear
-- gradient's control vector is from (x0,y0) to (x1,y1) while a radial gradient's
-- control vector is from any point on the start circle to the corresponding
-- point on the end circle.
--
-- The color is specified in the same way as in setSourceRGBA.
--
-- Note: If the pattern is not a gradient pattern, (eg. a linear or radial
-- pattern), then the pattern will be put into an error status with a status of
-- 'StatusPatternTypeMismatch'.
--
patternAddColorStopRGBA ::
     MonadIO m =>
     Pattern -- ^ a 'Pattern'
  -> Double  -- ^ an offset in the range [0.0 .. 1.0]
  -> Double  -- ^ red component of color
  -> Double  -- ^ green component of color
  -> Double  -- ^ blue component of color
  -> Double  -- ^ alpha component of color
  -> m ()
patternAddColorStopRGBA p offset r g b a = liftIO $ Internal.patternAddColorStopRGBA p offset r g b a

-- | Sets the pattern's transformation matrix to matrix. This matrix is a
-- transformation from user space to pattern space.
--
-- When a pattern is first created it always has the identity matrix for its
-- transformation matrix, which means that pattern space is initially identical
-- to user space.
--
-- Important: Please note that the direction of this transformation matrix is
-- from user space to pattern space. This means that if you imagine the flow
-- from a pattern to user space (and on to device space), then coordinates in
-- that flow will be transformed by the inverse of the pattern matrix.
--
-- Also, please note the discussion of the user-space locking semantics of 'setSource'.
--
patternSetMatrix ::
     MonadIO m =>
     Pattern -- ^ a 'Pattern'
  -> Matrix  -- ^ a 'Matrix'
  -> m ()
patternSetMatrix p m = liftIO $ Internal.patternSetMatrix p m

-- | Get the pattern's transformation matrix.
--
patternGetMatrix ::
     MonadIO m =>
     Pattern -- ^ a 'Pattern'
  -> m Matrix
patternGetMatrix p = liftIO $ Internal.patternGetMatrix p

-- |
--
patternSetExtend ::
     MonadIO m =>
     Pattern -- ^ a 'Pattern'
  -> Extend  -- ^ an 'Extent'
  -> m ()
patternSetExtend p e = liftIO $ Internal.patternSetExtend p e

-- |
--
patternGetExtend ::
     MonadIO m =>
     Pattern -- ^ a 'Pattern'
  -> m Extend
patternGetExtend p = liftIO $ Internal.patternGetExtend p

-- |
--
patternSetFilter ::
     MonadIO m =>
     Pattern -- ^ a 'Pattern'
  -> Filter  -- ^ a 'Filter'
  -> m ()
patternSetFilter p f = liftIO $ Internal.patternSetFilter p f

-- |
--
patternGetFilter ::
     MonadIO m =>
     Pattern -- ^ a 'Pattern'
  -> m Filter
patternGetFilter p = liftIO $ Internal.patternGetFilter p


-- | Modifies the current transformation matrix (CTM) by translating the
-- user-space origin by @(tx, ty)@. This offset is interpreted as a user-space
-- coordinate according to the CTM in place before the new call to 'translate'.
-- In other words, the translation of the user-space origin takes place after
-- any existing transformation.
--
translate ::
     Double -- ^ @tx@ - amount to translate in the X direction
  -> Double -- ^ @ty@ - amount to translate in the Y direction
  -> Render ()
translate = liftRender2 Internal.translate

-- | Modifies the current transformation matrix (CTM) by scaling the X and Y
-- user-space axes by sx and sy respectively. The scaling of the axes takes
-- place after any existing transformation of user space.
--
scale ::
     Double -- ^ @sx@ - scale factor for the X dimension
  -> Double -- ^ @sy@ - scale factor for the Y dimension
  -> Render ()
scale = liftRender2 Internal.scale

-- | Modifies the current transformation matrix (CTM) by rotating the user-space
-- axes by @angle@ radians. The rotation of the axes takes places after any
-- existing transformation of user space. The rotation direction for positive
-- angles is from the positive X axis toward the positive Y axis.
--
rotate ::
     Double -- ^ @angle@ - angle (in radians) by which the user-space axes will
            -- be rotated
  -> Render ()
rotate = liftRender1 Internal.rotate

-- | Modifies the current transformation matrix (CTM) by applying matrix as an
-- additional transformation. The new transformation of user space takes place
-- after any existing transformation.
--
transform ::
     Matrix -- ^ @matrix@ - a transformation to be applied to the user-space axes
  -> Render ()
transform = liftRender1 Internal.transform

-- | Modifies the current transformation matrix (CTM) by setting it equal to
-- @matrix@.
setMatrix ::
     Matrix -- ^ @matrix@ - a transformation matrix from user space to device space
  -> Render ()
setMatrix = liftRender1 Internal.setMatrix

-- | Gets the current transformation matrix, as set by 'setMatrix'.
--
getMatrix :: Render Matrix
getMatrix = liftRender0 Internal.getMatrix

-- | Resets the current transformation matrix (CTM) by setting it equal to the
-- identity matrix. That is, the user-space and device-space axes will be
-- aligned and one user-space unit will transform to one device-space unit.
--
identityMatrix :: Render ()
identityMatrix = liftRender0 Internal.identityMatrix

-- | Transform a coordinate from user space to device space by multiplying the
-- given point by the current transformation matrix (CTM).
--
userToDevice ::
     Double -- ^ X value of coordinate
  -> Double -- ^ Y value of coordinate
  -> Render (Double,Double)
userToDevice = liftRender2 Internal.userToDevice

-- | Transform a distance vector from user space to device space. This function
-- is similar to 'userToDevice' except that the translation components of the
-- CTM will be ignored when transforming @(dx,dy)@.
--
userToDeviceDistance ::
     Double -- ^ @dx@ - X component of a distance vector
  -> Double -- ^ @dy@ - Y component of a distance vector
  -> Render (Double,Double)
userToDeviceDistance = liftRender2 Internal.userToDeviceDistance

-- | Transform a coordinate from device space to user space by multiplying the
-- given point by the inverse of the current transformation matrix (CTM).
--
deviceToUser ::
     Double -- ^ X value of coordinate
  -> Double -- ^ Y value of coordinate
  -> Render (Double,Double)
deviceToUser = liftRender2 Internal.deviceToUser

-- | Transform a distance vector from device space to user space. This function
-- is similar to 'deviceToUser' except that the translation components of the
-- inverse CTM will be ignored when transforming @(dx,dy)@.
--
deviceToUserDistance ::
     Double -- ^ @dx@ - X component of a distance vector
  -> Double -- ^ @dy@ - Y component of a distance vector
  -> Render (Double,Double)
deviceToUserDistance = liftRender2 Internal.deviceToUserDistance


-- | Selects a family and style of font from a simplified description as a
-- @family@ name, @slant@ and @weight@. This function is meant to be used only
-- for applications with simple font needs: Cairo doesn't provide for operations
-- such as listing all available fonts on the system, and it is expected that
-- most applications will need to use a more comprehensive font handling and
-- text layout library in addition to cairo.
--
selectFontFace ::
     CairoString string
  => string     -- ^ @family@ - a font family name
  -> FontSlant  -- ^ @slant@ - the slant for the font
  -> FontWeight -- ^ @weight@ - the weight of the font
  -> Render ()
selectFontFace = liftRender3 Internal.selectFontFace

-- | Sets the current font matrix to a scale by a factor of @size@, replacing
-- any font matrix previously set with 'setFontSize' or 'setFontMatrix'. This
-- results in a font size of size user space units. (More precisely, this matrix
-- will result in the font's em-square being a size by size square in user space.)
--
setFontSize ::
     Double -- ^ @size@ - the new font size, in user space units
  -> Render ()
setFontSize = liftRender1 Internal.setFontSize

-- | Sets the current font matrix to @matrix@. The font matrix gives a
-- transformation from the design space of the font (in this space, the
-- em-square is 1 unit by 1 unit) to user space. Normally, a simple scale is
-- used (see 'setFontSize'), but a more complex font matrix can be used to shear
-- the font or stretch it unequally along the two axes.
--
setFontMatrix ::
     Matrix -- ^ @matrix@ - a 'Matrix' describing a transform to be applied to
            -- the current font.
  -> Render ()
setFontMatrix = liftRender1 Internal.setFontMatrix

-- | Gets the current font matrix, as set by 'setFontMatrix'
--
getFontMatrix :: Render Matrix
getFontMatrix = liftRender0 Internal.getFontMatrix

-- | Sets a set of custom font rendering options. Rendering options are
-- derived by merging these options with the options derived from underlying
-- surface; if the value in @options@ has a default value (like
-- 'AntialiasDefault'), then the value from the surface is used.
--
setFontOptions :: FontOptions -> Render ()
setFontOptions = liftRender1 Internal.setFontOptions
        
-- | A drawing operator that generates the shape from a string of Unicode
-- characters, rendered according to the current font face, font size (font
-- matrix), and font options.
--
-- This function first computes a set of glyphs for the string of text. The
-- first glyph is placed so that its origin is at the current point. The origin
-- of each subsequent glyph is offset from that of the previous glyph by the
-- advance values of the previous glyph.
--
-- After this call the current point is moved to the origin of where the next
-- glyph would be placed in this same progression. That is, the current point
-- will be at the origin of the final glyph offset by its advance values. This
-- allows for easy display of a single logical string with multiple calls to
-- 'showText'.
--
-- NOTE: The 'showText' function call is part of what the cairo designers call
-- the \"toy\" text API. It is convenient for short demos and simple programs,
-- but it is not expected to be adequate for the most serious of text-using
-- applications.
--
showText ::
     CairoString string
  => string -- ^ a string of text
  -> Render ()
showText = liftRender1 Internal.showText

-- | Gets the font extents for the currently selected font.
--
fontExtents :: Render FontExtents
fontExtents = liftRender0 Internal.fontExtents

-- | Gets the extents for a string of text. The extents describe a user-space
-- rectangle that encloses the \"inked\" portion of the text, (as it would be
-- drawn by 'showText'). Additionally, the 'textExtentsXadvance' and
-- 'textExtentsYadvance' values indicate the amount by which the current point
-- would be advanced by 'showText'.
--
-- Note that whitespace characters do not directly contribute to the size of
-- the rectangle ('textExtentsWidth' and 'textExtentsHeight'). They do contribute
-- indirectly by changing the position of non-whitespace characters.
-- In particular, trailing whitespace characters are likely to not affect the
-- size of the rectangle, though they will affect the 'textExtentsXadvance' and
-- 'textExtentsYadvance' values.
--
textExtents ::
     CairoString string
  => string -- ^ a string of text
  -> Render TextExtents
textExtents = liftRender1 Internal.textExtents


-- | Allocates a new font options object with all options initialized to default
-- values.
--
fontOptionsCreate :: MonadIO m => m FontOptions
fontOptionsCreate = liftIO $ Internal.fontOptionsCreate

-- | Allocates a new font options object copying the option values from @original@.
--
fontOptionsCopy ::
     MonadIO m =>
     FontOptions -- ^ @original@
  -> m FontOptions
fontOptionsCopy a = liftIO $ Internal.fontOptionsCopy a

-- | Merges non-default options from @other@ into @options@, replacing existing
-- values. This operation can be thought of as somewhat similar to compositing
-- @other@ onto @options@ with the operation of 'OperationOver'.
--
fontOptionsMerge ::
     MonadIO m =>
     FontOptions -- ^ @options@
  -> FontOptions -- ^ @other@
  -> m ()
fontOptionsMerge a b = liftIO $ Internal.fontOptionsMerge a b

-- | Compute a hash for the font options object; this value will be useful when
-- storing an object containing a 'FontOptions' in a hash table.
--
fontOptionsHash :: MonadIO m => FontOptions -> m Int
fontOptionsHash a = liftIO $ Internal.fontOptionsHash a

-- | Compares two font options objects for equality.
--
fontOptionsEqual :: MonadIO m => FontOptions -> FontOptions -> m Bool
fontOptionsEqual a b = liftIO $ Internal.fontOptionsEqual a b

-- | Sets the antiliasing mode for the font options object. This specifies the
-- type of antialiasing to do when rendering text.
--
fontOptionsSetAntialias :: MonadIO m => FontOptions -> Antialias -> m ()
fontOptionsSetAntialias a b = liftIO $ Internal.fontOptionsSetAntialias a b

-- | Gets the antialising mode for the font options object.
--
fontOptionsGetAntialias :: MonadIO m => FontOptions -> m Antialias
fontOptionsGetAntialias a = liftIO $ Internal.fontOptionsGetAntialias a

-- | Sets the subpixel order for the font options object. The subpixel order
-- specifies the order of color elements within each pixel on the display device
-- when rendering with an antialiasing mode of 'AntialiasSubpixel'.
-- See the documentation for 'SubpixelOrder' for full details.
--
fontOptionsSetSubpixelOrder :: MonadIO m => FontOptions -> SubpixelOrder-> m ()
fontOptionsSetSubpixelOrder a b = liftIO $ Internal.fontOptionsSetSubpixelOrder a b

-- | Gets the subpixel order for the font options object.
-- See the documentation for 'SubpixelOrder' for full details.
--
fontOptionsGetSubpixelOrder :: MonadIO m => FontOptions -> m SubpixelOrder
fontOptionsGetSubpixelOrder a = liftIO $ Internal.fontOptionsGetSubpixelOrder a

-- | Sets the hint style for font outlines for the font options object.
-- This controls whether to fit font outlines to the pixel grid, and if so,
-- whether to optimize for fidelity or contrast. See the documentation for
-- 'HintStyle' for full details.
--
fontOptionsSetHintStyle :: MonadIO m => FontOptions -> HintStyle -> m ()
fontOptionsSetHintStyle a b = liftIO $ Internal.fontOptionsSetHintStyle a b

-- | Gets the hint style for font outlines for the font options object.
-- See the documentation for 'HintStyle' for full details.
--
fontOptionsGetHintStyle :: MonadIO m => FontOptions -> m HintStyle
fontOptionsGetHintStyle a = liftIO $ Internal.fontOptionsGetHintStyle a

-- | Sets the metrics hinting mode for the font options object. This controls
-- whether metrics are quantized to integer values in device units. See the
-- documentation for 'HintMetrics' for full details.
--
fontOptionsSetHintMetrics :: MonadIO m => FontOptions -> HintMetrics -> m ()
fontOptionsSetHintMetrics a b = liftIO $ Internal.fontOptionsSetHintMetrics a b

-- | Gets the metrics hinting mode for the font options object. See the
-- documentation for 'HintMetrics' for full details.
--
fontOptionsGetHintMetrics :: MonadIO m => FontOptions -> m HintMetrics
fontOptionsGetHintMetrics a = liftIO $ Internal.fontOptionsGetHintMetrics a


-- | Create a temporary surface that is as compatible as possible with an
-- existing surface. The new surface will use the same backend as other unless
-- that is not possible for some reason.
--
withSimilarSurface ::
     Surface -- ^ an existing surface used to select the backend of the new surface
  -> Content -- ^ the content type for the new surface (color, color+alpha or alpha only)
  -> Int     -- ^ width of the new surface, (in device-space units)
  -> Int     -- ^ height of the new surface (in device-space units)
  -> (Surface -> IO a)
  -> IO a
withSimilarSurface surface contentType width height f =
  bracket (Internal.surfaceCreateSimilar surface contentType width height)
          (\surface' -> do status <- Internal.surfaceStatus surface'
                           Internal.surfaceDestroy surface'
                           unless (status == StatusSuccess) $
                             Internal.statusToString status >>= fail)
          (\surface' -> f surface')


-- | Like 'withSimilarSurface' but creates a Surface that is managed by the
-- Haskell memory manager rather than only being temporaily allocated. This
-- is more flexible and allows you to create surfaces that persist, which
-- can be very useful, for example to cache static elements in an animation.
--
-- However you should be careful because surfaces can be expensive resources
-- and the Haskell memory manager cannot guarantee when it will release them.
-- You can manually release the resources used by a surface with
-- 'surfaceFinish'.
--
createSimilarSurface ::
     Surface -- ^ an existing surface used to select the backend of the new surface
  -> Content -- ^ the content type for the new surface (color, color+alpha or alpha only)
  -> Int     -- ^ width of the surface, in pixels
  -> Int     -- ^ height of the surface, in pixels
  -> IO Surface
createSimilarSurface surface contentType width height = do
  surface <- Internal.surfaceCreateSimilar surface contentType width height
  Internal.manageSurface surface
  return surface

-- | Create a temporary surface that is compatible with the current target
-- surface (like a combination of 'withTargetSurface' and 'withSimilarSurface').
--
-- This is useful for drawing to a temporary surface and then compositing it
-- into the main suface. For example, the following code draws to a temporary
-- surface and then uses that as a mask:
--
-- > renderWithSimilarSurface ContentAlpha 200 200 $ \tmpSurface -> do
-- >   renderWith tmpSurface $ do
-- >     ... -- draw onto the temporary surface
-- >
-- >   -- use the temporary surface as a mask, filling it with the
-- >   -- current source which in this example is transparent red.
-- >   setSourceRGBA 1 0 0 0.5
-- >   setOperator Operator{something} -- think of something clever to do
-- >   maskSurface tmpSurface 0 0)
--
renderWithSimilarSurface ::
     Content -- ^ the content type for the new surface
             -- (color, colour+alpha or alpha only)
  -> Int     -- ^ width of the new surface, (in device-space units)
  -> Int     -- ^ height of the new surface, (in device-space units)
  -> (Surface -> Render a) -- ^ this action draws on the main surface,
                           -- possibly making use of the temporary surface
                           -- (which gets destroyed afterwards).
  -> Render a
renderWithSimilarSurface contentType width height render =
  withTargetSurface $ \surface ->
  bracketR (Internal.surfaceCreateSimilar surface contentType width height)
           (\surface' -> do status <- Internal.surfaceStatus surface'
                            Internal.surfaceDestroy surface'
                            unless (status == StatusSuccess) $
                              Internal.statusToString status >>= fail)
           (\surface' -> render surface')

-- | This function finishes the surface and drops all references to external
-- resources. For example, for the Xlib backend it means that cairo will no
-- longer access the drawable, which can be freed. After calling 'surfaceFinish'
-- the only valid operations on a surface are getting and setting user data and
-- referencing and destroying it. Further drawing to the surface will not affect
-- the surface but will instead trigger a 'StatusSurfaceFinished' error.
--
-- When the last call to 'surfaceDestroy' decreases the reference count to zero,
-- cairo will call 'surfaceFinish' if it hasn't been called already, before
-- freeing the resources associated with the surface.
--
surfaceFinish :: MonadIO m => Surface -> m ()
surfaceFinish surface = liftIO $ do
  status <- Internal.surfaceStatus surface
  Internal.surfaceFinish surface
  unless (status == StatusSuccess) $
    Internal.statusToString status >>= fail

-- | Do any pending drawing for the surface and also restore any temporary
-- modification's cairo has made to the surface's state. This function must be
-- called before switching from drawing on the surface with cairo to drawing on
-- it directly with native APIs. If the surface doesn't support direct access,
-- then this function does nothing.
--
surfaceFlush :: MonadIO m => Surface -> m ()
surfaceFlush a = liftIO $ Internal.surfaceFlush a

-- | Retrieves the default font rendering options for the surface. This allows
-- display surfaces to report the correct subpixel order for rendering on them,
-- print surfaces to disable hinting of metrics and so forth. The result can
-- then be used with 'scaledFontCreate'.
--
surfaceGetFontOptions :: Surface -> Render FontOptions
surfaceGetFontOptions surface = do
  fontOptions <- fontOptionsCreate
  liftIO $ Internal.surfaceGetFontOptions surface fontOptions
  return fontOptions

-- | Tells cairo that drawing has been done to surface using means other than
-- cairo, and that cairo should reread any cached areas. Note that you must call
-- 'surfaceFlush' before doing such drawing.
--
surfaceMarkDirty :: MonadIO m => Surface -> m ()
surfaceMarkDirty a = liftIO $ Internal.surfaceMarkDirty a

-- | Like 'surfaceMarkDirty', but drawing has been done only to the specified
-- rectangle, so that cairo can retain cached contents for other parts of the
-- surface.
--
surfaceMarkDirtyRectangle ::
     MonadIO m =>
     Surface -- ^ a 'Surface'
  -> Int     -- ^ X coordinate of dirty rectangle
  -> Int     -- ^ Y coordinate of dirty rectangle
  -> Int     -- ^ width of dirty rectangle
  -> Int     -- ^ height of dirty rectangle
  -> m ()
surfaceMarkDirtyRectangle a b c d e = liftIO $ Internal.surfaceMarkDirtyRectangle a b c d e

-- | Sets an offset that is added to the device coordinates determined by the
-- CTM when drawing to surface. One use case for this function is when we want
-- to create a 'Surface' that redirects drawing for a portion of an
-- onscreen surface to an offscreen surface in a way that is completely
-- invisible to the user of the cairo API. Setting a transformation via
-- 'translate' isn't sufficient to do this, since functions like 'deviceToUser'
-- will expose the hidden offset.
--
-- Note that the offset only affects drawing to the surface, not using the
-- surface in a surface pattern.
--
surfaceSetDeviceOffset ::
     MonadIO m =>
     Surface -- ^ a 'Surface'
  -> Double  -- ^ the offset in the X direction, in device units
  -> Double  -- ^ the offset in the Y direction, in device units
  -> m ()
surfaceSetDeviceOffset a b c = liftIO $ Internal.surfaceSetDeviceOffset a b c

#if CAIRO_CHECK_VERSION(1,6,0)
-- | This function provides a stride value that will respect all alignment
--   requirements of the accelerated image-rendering code within cairo.
--
formatStrideForWidth ::
     Format -- ^ format of pixels in the surface to create
  -> Int    -- ^ width of the surface, in pixels
  -> Int    -- ^ the stride (number of bytes necessary to store one line)
            --   or @-1@ if the format is invalid or the width is too large
formatStrideForWidth = Internal.formatStrideForWidth
#endif

-- | Creates an image surface of the specified format and dimensions.
-- The initial contents of the surface is undefined; you must explicitely
-- clear the buffer, using, for example, 'rectangle' and 'fill' if you want it
-- cleared.
--
withImageSurface ::
     Format -- ^ format of pixels in the surface to create
  -> Int    -- ^ width of the surface, in pixels
  -> Int    -- ^ height of the surface, in pixels
  -> (Surface -> IO a) -- ^ an action that may use the surface. The surface is
                       -- only valid within in this action.
  -> IO a
withImageSurface format width height f =
  bracket (Internal.imageSurfaceCreate format width height)
          (\surface -> do status <- Internal.surfaceStatus surface
                          Internal.surfaceDestroy surface
                          unless (status == StatusSuccess) $
                            Internal.statusToString status >>= fail)
          (\surface -> f surface)

-- | Like 'withImageSurface' but creates a Surface that is managed by the
-- Haskell memory manager rather than only being temporaily allocated. This
-- is more flexible and allows you to create surfaces that persist, which
-- can be very useful, for example to cache static elements in an animation.
--
-- However you should be careful because surfaces can be expensive resources
-- and the Haskell memory manager cannot guarantee when it will release them.
-- You can manually release the resources used by a surface with
-- 'surfaceFinish'.
--
createImageSurface ::
     Format -- ^ format of pixels in the surface to create
  -> Int    -- ^ width of the surface, in pixels
  -> Int    -- ^ height of the surface, in pixels
  -> IO Surface
createImageSurface format width height = do
  surface <- Internal.imageSurfaceCreate format width height
  Internal.manageSurface surface
  return surface

-- | Like 'withImageSurface' but creating a surface to target external
-- data pointed to by 'PixelData'.
--
withImageSurfaceForData ::
     PixelData         -- ^ pointer to pixel data
  -> Format            -- ^ format of pixels in the surface to create
  -> Int               -- ^ width of the surface, in pixels
  -> Int               -- ^ height of the surface, in pixels
  -> Int               -- ^ size of stride between rows in the surface to create
  -> (Surface -> IO a) -- ^ an action that may use the surface. The surface is
                       -- only valid within this action
  -> IO a
withImageSurfaceForData pixels format width height stride f =
  bracket (Internal.imageSurfaceCreateForData pixels format width height stride)
          (\surface -> do status <- Internal.surfaceStatus surface
                          Internal.surfaceDestroy surface
                          unless (status == StatusSuccess) $
                            Internal.statusToString status >>= fail)
          (\surface -> f surface)

-- | Like 'createImageSurface' but creating a surface to target external
-- data pointed to by 'PixelData'.
--
createImageSurfaceForData ::
     PixelData         -- ^ pointer to pixel data
  -> Format            -- ^ format of pixels in the surface to create
  -> Int               -- ^ width of the surface, in pixels
  -> Int               -- ^ height of the surface, in pixels
  -> Int               -- ^ size of stride between rows in the surface to create
  -> IO Surface
createImageSurfaceForData pixels format width height stride = do
  surface <- Internal.imageSurfaceCreateForData pixels format width height stride
  Internal.manageSurface surface
  return surface

-- | Get the width of the image surface in pixels.
--
imageSurfaceGetWidth :: MonadIO m => Surface -> m Int
imageSurfaceGetWidth a = liftIO $ Internal.imageSurfaceGetWidth a

-- | Get the height of the image surface in pixels.
--
imageSurfaceGetHeight :: MonadIO m => Surface -> m Int
imageSurfaceGetHeight a = liftIO $ Internal.imageSurfaceGetHeight a

#if CAIRO_CHECK_VERSION(1,2,0)
-- | Get the number of bytes from the start of one row to the start of the
--   next. If the image data contains no padding, then this is equal to
--   the pixel depth * the width.
imageSurfaceGetStride :: MonadIO m => Surface -> m Int
imageSurfaceGetStride = liftIO . Internal.imageSurfaceGetStride


-- | Get the format of the surface.
--
imageSurfaceGetFormat :: MonadIO m => Surface -> m Format
imageSurfaceGetFormat a = liftIO $ Internal.imageSurfaceGetFormat a

#if __GLASGOW_HASKELL__ >= 606
-- | Return a ByteString of the image data for a surface. In order to remain
--   safe the returned ByteString is a copy of the data. This is a little
--   slower than returning a pointer into the image surface object itself, but
--   much safer
imageSurfaceGetData :: Surface -> IO BS.ByteString
imageSurfaceGetData a = do
  height <- Internal.imageSurfaceGetHeight a
  stride <- Internal.imageSurfaceGetStride a
  ptr <- Internal.imageSurfaceGetData a
#if __GLASGOW_HASKELL__ < 608
  BS.copyCStringLen (castPtr ptr, height * stride)
#else
  BS.packCStringLen (castPtr ptr, height * stride)
#endif
#endif


-- | Retrieve the internal array of raw image data.
--
-- * Image data in an image surface is stored in memory in uncompressed,
--   packed format. Rows in the image are stored top to bottom, and in each
--   row pixels are stored from left to right. There may be padding at the end
--   of a row. The value returned by 'imageSurfaceGetStride' indicates the
--   number of bytes between rows.
--
-- * The returned array is a flat representation of a three dimensional array:
--   x-coordiante, y-coordinate and several channels for each color. The
--   format depends on the 'Format' of the surface:
--
--   'FormatARGB32': each pixel is 32 bits with alpha in the upper 8 bits,
--    followed by 8 bits for red, green and blue. Pre-multiplied alpha is used.
--    (That is, 50% transparent red is 0x80800000, not 0x80ff0000.)
--
--   'FormatRGB24': each pixel is 32 bits with the upper 8 bits being unused,
--    followed by 8 bits for red, green and blue.
--
--   'FormatA8': each pixel is 8 bits holding an alpha value
--
--   'FormatA1': each pixel is one bit where pixels are packed into 32 bit
--   quantities. The ordering depends on the endianes of the platform. On a
--   big-endian machine, the first pixel is in the uppermost bit, on a
--   little-endian machine the first pixel is in the least-significant bit.
--
-- * To read or write a specific pixel (and assuming 'FormatARGB32' or
--   'FormatRGB24'), use the formula: @p = y * (rowstride `div` 4) + x@ for the
--   pixel and force the array to have 32-bit words or integers.
--
-- * Calling this function without explicitly giving it a type will often lead
--   to a compiler error since the type parameter @e@ is underspecified. If
--   this happens the function can be explicitly typed:
--   @surData <- (imageSurfaceGetPixels pb :: IO (SurfaceData Int Word32))@
--
-- * If modifying an image through Haskell\'s array interface is not fast
--   enough, it is possible to use 'unsafeRead' and 'unsafeWrite' which have
--   the same type signatures as 'readArray' and 'writeArray'. Note that these
--   are internal functions that might change with GHC.
--
-- * After each write access to the array, you need to inform Cairo
--   about the area that has changed using 'surfaceMarkDirty'.
--
-- * The function will return an error if the surface is not an image
--   surface or if 'surfaceFinish' has been called on the surface.
--
imageSurfaceGetPixels :: Storable e => Surface -> IO (SurfaceData Int e)
imageSurfaceGetPixels pb = do
  pixPtr <- Internal.imageSurfaceGetData pb
  when (pixPtr==nullPtr) $ do
    fail "imageSurfaceGetPixels: image surface not available"
  h <- imageSurfaceGetHeight pb
  r <- imageSurfaceGetStride pb
  return (mkSurfaceData pb (castPtr pixPtr) (h*r))

-- | An array that stores the raw pixel data of an image 'Surface'.
--
data SurfaceData i e = SurfaceData !Surface
                          {-# UNPACK #-} !(Ptr e)
                                         !(i,i)
                          {-# UNPACK #-} !Int

mkSurfaceData :: Storable e => Surface -> Ptr e -> Int -> SurfaceData Int e
mkSurfaceData pb (ptr :: Ptr e) size =
  SurfaceData pb ptr (0, count - 1) count
  where count = fromIntegral (size `div` sizeOf (undefined :: e)) :: Int

#if __GLASGOW_HASKELL__ < 605
instance HasBounds SurfaceData where
  bounds (SurfaceData pb ptr bd cnt) = bd
#endif

-- | 'SurfaceData' is a mutable array.
instance Storable e => MArray SurfaceData e IO where
  newArray (l,u) e = error "GI.Cairo.Render.newArray: not implemented"
  newArray_ (l,u)  = error "GI.Cairo.Render.newArray_: not implemented"
  {-# INLINE unsafeRead #-}
  unsafeRead (SurfaceData (Surface pb) pixPtr _ _) idx = do
      e <- peekElemOff pixPtr idx
      touchForeignPtr pb
      return e
  {-# INLINE unsafeWrite #-}
  unsafeWrite (SurfaceData (Surface pb) pixPtr _ _) idx elem = do
      pokeElemOff pixPtr idx elem
      touchForeignPtr pb
#if __GLASGOW_HASKELL__ >= 605
  {-# INLINE getBounds #-}
  getBounds (SurfaceData _ _ bd _) = return bd
#endif
#if __GLASGOW_HASKELL__ >= 608
  {-# INLINE getNumElements #-}
  getNumElements (SurfaceData _ _ _ count) = return count
#endif


#endif

#ifdef CAIRO_HAS_PDF_SURFACE
-- | Creates a PostScript surface of the specified size in points to
-- be written to @filename@.
--
-- Note that the size of individual pages of the PostScript output can
-- vary.  See 'psSurfaceSetSize'.
--
withPDFSurface ::
     FilePath -- ^ @filename@ - a filename for the PS output (must be writable)
  -> Double   -- ^ width of the surface, in points (1 point == 1\/72.0 inch)
  -> Double   -- ^ height of the surface, in points (1 point == 1\/72.0 inch)
  -> (Surface -> IO a) -- ^ an action that may use the surface. The surface is
                       -- only valid within in this action.
  -> IO a
withPDFSurface filename width height f = do
  surface <- Internal.pdfSurfaceCreate filename width height
  ret <- f surface
  Internal.surfaceDestroy surface
  return ret

#if CAIRO_CHECK_VERSION(1,2,0)
-- | Changes the size of a PDF surface for the current (and
-- subsequent) pages.
--
-- This function should only be called before any drawing operations
-- have been performed on the current page. The simplest way to do
-- this is to call this function immediately after creating the
-- surface or immediately after completing a page with either
-- 'showPage' or 'copyPage'.
--
pdfSurfaceSetSize :: MonadIO m => Surface -> Double -> Double -> m ()
pdfSurfaceSetSize s x y = liftIO $ Internal.pdfSurfaceSetSize s x y
#endif
#endif

#ifdef CAIRO_HAS_PNG_FUNCTIONS
-- | Creates a new image surface and initializes the contents to the given PNG
-- file.
--
withImageSurfaceFromPNG :: FilePath -> (Surface -> IO a) -> IO a
withImageSurfaceFromPNG filename f =
  bracket (Internal.imageSurfaceCreateFromPNG filename)
          (\surface -> do status <- Internal.surfaceStatus surface
                          Internal.surfaceDestroy surface
                          unless (status == StatusSuccess) $
                            Internal.statusToString status >>= fail)
          (\surface -> f surface)

-- | Writes the contents of surface to a new file @filename@ as a PNG image.
--
surfaceWriteToPNG ::
     Surface  -- ^ a 'Surface'
  -> FilePath -- ^ @filename@ - the name of a file to write to
  -> IO ()
surfaceWriteToPNG surface filename = do
  status <- Internal.surfaceWriteToPNG surface filename
  unless (status == StatusSuccess) $
    fail =<< Internal.statusToString status
  return ()
#endif

#ifdef CAIRO_HAS_PS_SURFACE
-- | Creates a PostScript surface of the specified size in points to
-- be written to @filename@.
--
-- Note that the size of individual pages of the PostScript output can
-- vary.  See 'psSurfaceSetSize'.
--
withPSSurface ::
     FilePath -- ^ @filename@ - a filename for the PS output (must be writable)
  -> Double   -- ^ width of the surface, in points (1 point == 1\/72.0 inch)
  -> Double   -- ^ height of the surface, in points (1 point == 1\/72.0 inch)
  -> (Surface -> IO a) -- ^ an action that may use the surface. The surface is
                       -- only valid within in this action.
  -> IO a
withPSSurface filename width height f =
  bracket (Internal.psSurfaceCreate filename width height)
          (\surface -> do status <- Internal.surfaceStatus surface
                          Internal.surfaceDestroy surface
                          unless (status == StatusSuccess) $
                            Internal.statusToString status >>= fail)
          (\surface -> f surface)

#if CAIRO_CHECK_VERSION(1,2,0)
-- | Changes the size of a PostScript surface for the current (and
-- subsequent) pages.
--
-- This function should only be called before any drawing operations
-- have been performed on the current page. The simplest way to do
-- this is to call this function immediately after creating the
-- surface or immediately after completing a page with either
-- 'showPage' or 'copyPage'.
--
psSurfaceSetSize :: MonadIO m => Surface -> Double -> Double -> m ()
psSurfaceSetSize s x y = liftIO $ Internal.psSurfaceSetSize s x y
#endif
#endif


#ifdef CAIRO_HAS_SVG_SURFACE
-- | Creates a SVG surface of the specified size in points
-- be written to @filename@.
--
withSVGSurface ::
     FilePath -- ^ @filename@ - a filename for the SVG output (must be writable)
  -> Double   -- ^ width of the surface, in points (1 point == 1\/72.0 inch)
  -> Double   -- ^ height of the surface, in points (1 point == 1\/72.0 inch)
  -> (Surface -> IO a) -- ^ an action that may use the surface. The surface is
                       -- only valid within in this action.
  -> IO a
withSVGSurface filename width height f =
  bracket (Internal.svgSurfaceCreate filename width height)
          (\surface -> do status <- Internal.surfaceStatus surface
                          Internal.surfaceDestroy surface
                          unless (status == StatusSuccess) $
                            Internal.statusToString status >>= fail)
          (\surface -> f surface)
#endif

#if CAIRO_CHECK_VERSION(1,10,0)

-- | Allocates a new empty region object.
--
regionCreate :: MonadIO m => m Region
regionCreate = liftIO $ Internal.regionCreate

-- | Allocates a new region object containing @rectangle@.
--
regionCreateRectangle ::
     MonadIO m =>
     RectangleInt -- ^ @rectangle@
  -> m Region
regionCreateRectangle a = liftIO $ Internal.regionCreateRectangle a

-- | Allocates a new region object containing the union of all given @rects@.
--
regionCreateRectangles ::
     MonadIO m =>
     [RectangleInt] -- ^ @rects@
  -> m Region
regionCreateRectangles a = liftIO $ Internal.regionCreateRectangles a

-- | Allocates a new region object copying the area from @original@.
--
regionCopy ::
     MonadIO m =>
     Region -- ^ @original@
  -> m Region
regionCopy a = liftIO $ Internal.regionCopy a

-- | Gets the bounding rectangle of @region@ as a RectanglInt.
--
regionGetExtents ::
     MonadIO m =>
     Region -- ^ @region@
  -> m RectangleInt
regionGetExtents a = liftIO $ Internal.regionGetExtents a

-- | Returns the number of rectangles contained in @region@.
--
regionNumRectangles ::
     MonadIO m =>
     Region -- ^ @region@
  -> m Int
regionNumRectangles a = liftIO $ Internal.regionNumRectangles a

-- | Gets the @nth@ rectangle from the @region@.
--
regionGetRectangle ::
     MonadIO m =>
     Region -- ^ @region@
  -> Int    -- ^ @nth@
  -> m RectangleInt
regionGetRectangle a n = liftIO $ Internal.regionGetRectangle a n

-- | Checks whether @region@ is empty.
--
regionIsEmpty ::
     MonadIO m =>
     Region -- ^ @region@
  -> m Bool
regionIsEmpty a = liftIO $ Internal.regionIsEmpty a

-- | Checks whether (@x@, @y@) is contained in @region@.
--
regionContainsPoint ::
     MonadIO m =>
     Region -- ^ @region@
  -> Int    -- ^ @x@
  -> Int    -- ^ @y@
  -> m Bool
regionContainsPoint a x y = liftIO $ Internal.regionContainsPoint a x y

-- | Checks whether @rectangle@ is inside, outside or partially contained in @region@.
--
regionContainsRectangle ::
     MonadIO m =>
     Region       -- ^ @region@
  -> RectangleInt -- ^ @rectangle@
  -> m RegionOverlap
regionContainsRectangle a rect = liftIO $ Internal.regionContainsRectangle a rect

-- | Compares whether @region_a@ is equivalent to @region_b@.
--
regionEqual ::
     MonadIO m =>
     Region -- ^ @region_a@
  -> Region -- ^ @region_b@
  -> m Bool
regionEqual a b = liftIO $ Internal.regionEqual a b

-- | Translates @region@ by (@dx@, @dy@).
--
regionTranslate ::
     MonadIO m =>
     Region -- ^ @region@
  -> Int    -- ^ @dx@
  -> Int    -- ^ @dy@
  -> m ()
regionTranslate a dx dy = liftIO $ Internal.regionTranslate a dx dy

-- | Computes the intersection of @dst@ with @other@ and places the result in @dst@.
--
regionIntersect ::
     MonadIO m =>
     Region -- ^ @dst@
  -> Region -- ^ @other@
  -> m ()
regionIntersect a b = liftIO $ do
  status <- Internal.regionIntersect a b
  unless (status == StatusSuccess) $
    Internal.statusToString status >>= fail

-- | Computes the intersection of @dst@ with @rectangle@ and places the result in @dst@.
--
regionIntersectRectangle ::
     MonadIO m =>
     Region       -- ^ @dst@
  -> RectangleInt -- ^ @rectangle@
  -> m ()
regionIntersectRectangle a rect = liftIO $ do
  status <- Internal.regionIntersectRectangle a rect
  unless (status == StatusSuccess) $
    Internal.statusToString status >>= fail

-- | Subtracts @other@ from @dst@ and places the result in @dst@.
--
regionSubtract ::
     MonadIO m =>
     Region -- ^ @dst@
  -> Region -- ^ @other@
  -> m ()
regionSubtract a b = liftIO $ do
  status <- Internal.regionSubtract a b
  unless (status == StatusSuccess) $
    Internal.statusToString status >>= fail

-- | Subtracts @rectangle@ from @dst@ and places the result in @dst@.
--
regionSubtractRectangle ::
     MonadIO m =>
     Region       -- ^ @dst@
  -> RectangleInt -- ^ @rectangle@
  -> m ()
regionSubtractRectangle a rect = liftIO $ do
  status <- Internal.regionSubtractRectangle a rect
  unless (status == StatusSuccess) $
    Internal.statusToString status >>= fail

-- | Computes the union of @dst@ with @other@ and places the result in @dst@.
--
regionUnion ::
     MonadIO m =>
     Region -- ^ @dst@
  -> Region -- ^ @other@
  -> m ()
regionUnion a b = liftIO $ do
  status <- Internal.regionUnion a b
  unless (status == StatusSuccess) $
    Internal.statusToString status >>= fail

-- | Computes the union of @dst@ with @rectangle@ and places the result in @dst@.
--
regionUnionRectangle ::
     MonadIO m =>
     Region       -- ^ @dst@
  -> RectangleInt -- ^ @rectangle@
  -> m ()
regionUnionRectangle a rect = liftIO $ do
  status <- Internal.regionUnionRectangle a rect
  unless (status == StatusSuccess) $
    Internal.statusToString status >>= fail

-- | Computes the exclusive difference of @dst@ with @other@ and places the result in @dst@.
-- That is, @dst@ will be set to contain all areas that are either in @dst@ or in @other@, but not in both.
--
regionXor ::
     MonadIO m =>
     Region -- ^ @dst@
  -> Region -- ^ @other@
  -> m ()
regionXor a b = liftIO $ do
  status <- Internal.regionXor a b
  unless (status == StatusSuccess) $
    Internal.statusToString status >>= fail

-- | Computes the exclusive difference of @dst@ with @rectangle@ and places the result in @dst@.
-- That is, @dst@ will be set to contain all areas that are either in @dst@ or in @rectangle@, but not in both
--
regionXorRectangle ::
     MonadIO m =>
     Region       -- ^ @dst@
  -> RectangleInt -- ^ @rectangle@
  -> m ()
regionXorRectangle a rect = liftIO $ do
  status <- Internal.regionXorRectangle a rect
  unless (status == StatusSuccess) $
    Internal.statusToString status >>= fail

#endif

-- | Returns the version of the cairo library encoded in a single integer.
--
version :: Int
version = Internal.version

-- | Returns the version of the cairo library as a human-readable string of the
-- form \"X.Y.Z\".
--
versionString :: String
versionString = Internal.versionString

