{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.Internal.Surfaces.SVG
-- Copyright   :  (c) Duncan Coutts 2007
-- License     :  BSD-style (see doc/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Rendering SVG images.
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo.Internal.Surfaces.SVG where

{#import Graphics.Rendering.Cairo.Types#}

import Foreign
import Foreign.C

{#context lib="cairo" prefix="cairo"#}

#ifdef CAIRO_HAS_SVG_SURFACE

{#fun svg_surface_create  as svgSurfaceCreate { withCAString* `FilePath', `Double', `Double' } -> `Surface' mkSurface*#}

#endif
