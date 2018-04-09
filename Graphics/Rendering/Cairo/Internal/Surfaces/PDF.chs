{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.Internal.Surfaces.PDF
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see doc/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Rendering PDF documents.
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo.Internal.Surfaces.PDF where

{#import Graphics.Rendering.Cairo.Types#}

import Foreign
import Foreign.C

{#context lib="cairo" prefix="cairo"#}

#ifdef CAIRO_HAS_PDF_SURFACE

{#fun pdf_surface_create  as pdfSurfaceCreate { withCAString* `FilePath', `Double', `Double' } -> `Surface' mkSurface*#}

#if CAIRO_CHECK_VERSION(1,2,0)
{#fun pdf_surface_set_size as pdfSurfaceSetSize { withSurface* `Surface', `Double', `Double' } -> `()'#}
#endif

#endif
