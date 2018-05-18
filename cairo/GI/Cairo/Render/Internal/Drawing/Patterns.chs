-----------------------------------------------------------------------------
-- |
-- Module      :  GI.Cairo.Render.Internal.Drawing.Patterns
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see cairo/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Gradients and filtered sources.
-----------------------------------------------------------------------------

#include "gi-cairo.h"

module GI.Cairo.Render.Internal.Drawing.Patterns where

{#import GI.Cairo.Render.Types#}

import Foreign
import Foreign.C
import qualified Foreign.C.Types as C2HSImp
import qualified Foreign.Ptr as C2HSImp
import qualified Foreign.Storable as C2HSImp 

import Data.GI.Base (withManagedPtr) 

{#context lib="cairo" prefix="cairo"#}

{#fun pattern_add_color_stop_rgb  as patternAddColorStopRGB  { `Pattern', `Double', `Double', `Double', `Double' } -> `()'#}
{#fun pattern_add_color_stop_rgba as patternAddColorStopRGBA { `Pattern', `Double', `Double', `Double', `Double', `Double' } -> `()'#}
{#fun pattern_create_rgb          as patternCreateRGB        { `Double', `Double', `Double' } -> `Pattern' #}
{#fun pattern_create_rgba         as patternCreateRGBA       { `Double', `Double', `Double', `Double' } -> `Pattern' #}
{#fun pattern_create_for_surface  as patternCreateForSurface { withManagedPtr* `Surface' } -> `Pattern'#}
{#fun pattern_create_linear       as patternCreateLinear     { `Double', `Double', `Double', `Double' } -> `Pattern'#}
{#fun pattern_create_radial       as patternCreateRadial     { `Double', `Double', `Double', `Double', `Double', `Double' } -> `Pattern'#}
{#fun pattern_destroy    as patternDestroy   { `Pattern' } -> `()'#}
{#fun pattern_reference  as patternReference { `Pattern' } -> `Pattern' #}
{#fun pattern_status     as patternStatus    { `Pattern' } -> `Status' cToEnum#}
{#fun pattern_set_extend as patternSetExtend { `Pattern', cFromEnum `Extend' } -> `()'#}
{#fun pattern_get_extend as patternGetExtend { `Pattern' } -> `Extend' cToEnum#}
{#fun pattern_set_filter as patternSetFilter { `Pattern', cFromEnum `Filter' } -> `()'#}
{#fun pattern_get_filter as patternGetFilter { `Pattern' } -> `Filter' cToEnum#}
{#fun pattern_set_matrix as patternSetMatrix { `Pattern', with* `Matrix' } -> `()'#}
{#fun pattern_get_matrix as patternGetMatrix { `Pattern', alloca- `Matrix' peek*} -> `()'#}
