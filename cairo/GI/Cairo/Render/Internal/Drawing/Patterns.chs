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

{#fun pattern_add_color_stop_rgb  as patternAddColorStopRGB  { withManagedPtr* `Pattern', `Double', `Double', `Double', `Double' } -> `()'#}
{#fun pattern_add_color_stop_rgba as patternAddColorStopRGBA { withManagedPtr* `Pattern', `Double', `Double', `Double', `Double', `Double' } -> `()'#}
{#fun pattern_create_rgb          as patternCreateRGB        { `Double', `Double', `Double' } -> `Pattern' mkPattern*#}
{#fun pattern_create_rgba         as patternCreateRGBA       { `Double', `Double', `Double', `Double' } -> `Pattern' mkPattern*#}
{#fun pattern_create_for_surface  as patternCreateForSurface { withManagedPtr* `Surface' } -> `Pattern' mkPattern* #}
{#fun pattern_create_linear       as patternCreateLinear     { `Double', `Double', `Double', `Double' } -> `Pattern' mkPattern*#}
{#fun pattern_create_radial       as patternCreateRadial     { `Double', `Double', `Double', `Double', `Double', `Double' } -> `Pattern' mkPattern*#}
{#fun pattern_destroy    as patternDestroy   { withManagedPtr* `Pattern' } -> `()'#}
{#fun pattern_reference  as patternReference { withManagedPtr* `Pattern' } -> `Pattern' mkPattern*#}
{#fun pattern_status     as patternStatus    { withManagedPtr* `Pattern' } -> `Status' cToEnum#}
{#fun pattern_set_extend as patternSetExtend { withManagedPtr* `Pattern', cFromEnum `Extend' } -> `()'#}
{#fun pattern_get_extend as patternGetExtend { withManagedPtr* `Pattern' } -> `Extend' cToEnum#}
{#fun pattern_set_filter as patternSetFilter { withManagedPtr* `Pattern', cFromEnum `Filter' } -> `()'#}
{#fun pattern_get_filter as patternGetFilter { withManagedPtr* `Pattern' } -> `Filter' cToEnum#}
{#fun pattern_set_matrix as patternSetMatrix { withManagedPtr* `Pattern', with* `Matrix' } -> `()'#}
{#fun pattern_get_matrix as patternGetMatrix { withManagedPtr* `Pattern', alloca- `Matrix' peek*} -> `()'#}
