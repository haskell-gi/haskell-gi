-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.Internal.Drawing.Patterns
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see cairo/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Gradients and filtered sources.
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo.Internal.Drawing.Patterns where

{#import Graphics.Rendering.Cairo.Types#}

import Foreign
import Foreign.C

{#context lib="cairo" prefix="cairo"#}

{#fun pattern_add_color_stop_rgb  as patternAddColorStopRGB  { unPattern `Pattern', `Double', `Double', `Double', `Double' } -> `()'#}
{#fun pattern_add_color_stop_rgba as patternAddColorStopRGBA { unPattern `Pattern', `Double', `Double', `Double', `Double', `Double' } -> `()'#}
{#fun pattern_create_rgb          as patternCreateRGB        { `Double', `Double', `Double' } -> `Pattern' Pattern#}
{#fun pattern_create_rgba         as patternCreateRGBA       { `Double', `Double', `Double', `Double' } -> `Pattern' Pattern#}
{#fun pattern_create_for_surface  as patternCreateForSurface { withSurface* `Surface' } -> `Pattern' Pattern#}
{#fun pattern_create_linear       as patternCreateLinear     { `Double', `Double', `Double', `Double' } -> `Pattern' Pattern#}
{#fun pattern_create_radial       as patternCreateRadial     { `Double', `Double', `Double', `Double', `Double', `Double' } -> `Pattern' Pattern#}
{#fun pattern_destroy    as patternDestroy   { unPattern `Pattern' } -> `()'#}
{#fun pattern_reference  as patternReference { unPattern `Pattern' } -> `Pattern' Pattern#}
{#fun pattern_status     as patternStatus    { unPattern `Pattern' } -> `Status' cToEnum#}
{#fun pattern_set_extend as patternSetExtend { unPattern `Pattern', cFromEnum `Extend' } -> `()'#}
{#fun pattern_get_extend as patternGetExtend { unPattern `Pattern' } -> `Extend' cToEnum#}
{#fun pattern_set_filter as patternSetFilter { unPattern `Pattern', cFromEnum `Filter' } -> `()'#}
{#fun pattern_get_filter as patternGetFilter { unPattern `Pattern' } -> `Filter' cToEnum#}
{#fun pattern_set_matrix as patternSetMatrix { unPattern `Pattern', `Matrix' } -> `()'#}
{#fun pattern_get_matrix as patternGetMatrix { unPattern `Pattern', alloca- `Matrix' peek*} -> `()'#}
