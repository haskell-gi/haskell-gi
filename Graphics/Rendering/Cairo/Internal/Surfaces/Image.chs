{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.Internal.Surfaces.Image
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see doc/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Rendering to memory buffers.
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo.Internal.Surfaces.Image where

{#import Graphics.Rendering.Cairo.Types#}

import Foreign
import Foreign.C

{#context lib="cairo" prefix="cairo"#}

{#fun image_surface_create_for_data as imageSurfaceCreateForData
  { id `Ptr CUChar'
  , cFromEnum `Format'
  , `Int'
  , `Int'
  , `Int'
  } -> `Surface' mkSurface*#}

{#fun image_surface_create     as imageSurfaceCreate    { cFromEnum `Format', `Int', `Int' } -> `Surface' mkSurface*#}
{#fun image_surface_get_width  as imageSurfaceGetWidth  { withSurface* `Surface' } -> `Int'#}
{#fun image_surface_get_height as imageSurfaceGetHeight { withSurface* `Surface' } -> `Int'#}
#if CAIRO_CHECK_VERSION(1,2,0)
{#fun image_surface_get_stride as imageSurfaceGetStride { withSurface* `Surface' } -> `Int'#}
{#fun image_surface_get_format as imageSurfaceGetFormat { withSurface* `Surface' } -> `Format' cToEnum#}
{#fun image_surface_get_data   as imageSurfaceGetData   { withSurface* `Surface' } -> `(Ptr CUChar)' id#}
#if CAIRO_CHECK_VERSION(1,6,0)
{#fun pure format_stride_for_width as formatStrideForWidth { cFromEnum `Format', `Int' } -> `Int'#}
#endif
#endif
