-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.Internal.Surfaces.Surface
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see doc/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Base class for surfaces.
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo.Internal.Surfaces.Surface where

{#import Graphics.Rendering.Cairo.Types#}

import Foreign
import Foreign.C

{#context lib="cairo" prefix="cairo"#}

{#fun surface_create_similar       as surfaceCreateSimilar      { withSurface* `Surface', cFromEnum `Content', `Int', `Int' } -> `Surface' mkSurface*#}
{#fun surface_destroy              as surfaceDestroy            { withSurface* `Surface' } -> `()'#}
{#fun surface_finish               as surfaceFinish             { withSurface* `Surface' } -> `()'#}
{#fun surface_flush                as surfaceFlush              { withSurface* `Surface' } -> `()'#}
{#fun surface_get_font_options     as surfaceGetFontOptions     { withSurface* `Surface', withFontOptions* `FontOptions'} -> `()'#}
{#fun surface_get_content          as surfaceGetContent         { withSurface* `Surface' } -> `Content' cToEnum#}
{#fun surface_mark_dirty           as surfaceMarkDirty          { withSurface* `Surface' } -> `()'#}
{#fun surface_mark_dirty_rectangle as surfaceMarkDirtyRectangle { withSurface* `Surface', `Int', `Int', `Int', `Int' } -> `()'#}
{#fun surface_reference            as surfaceReference          { withSurface* `Surface' } -> `()'#}
{#fun surface_set_device_offset    as surfaceSetDeviceOffset    { withSurface* `Surface', `Double', `Double' } -> `()'#}
{#fun surface_status               as surfaceStatus             { withSurface* `Surface' } -> `Status' cToEnum#}
