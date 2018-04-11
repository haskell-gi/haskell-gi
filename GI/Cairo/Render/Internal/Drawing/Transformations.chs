-----------------------------------------------------------------------------
-- |
-- Module      :  GI.Cairo.Render.Internal.Drawing.Tranformations
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see cairo/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Manipulating the current transformation matrix.
-----------------------------------------------------------------------------

#include "gi-cairo-render.h" 

module GI.Cairo.Render.Internal.Drawing.Transformations where

{#import GI.Cairo.Render.Types#}

import Foreign hiding (rotate)
import Foreign.C

{#context lib="cairo" prefix="cairo"#}

{#fun translate          as translate        { unCairo `Cairo', `Double', `Double' } -> `()'#}
{#fun scale              as scale            { unCairo `Cairo', `Double', `Double' } -> `()'#}
{#fun rotate             as rotate           { unCairo `Cairo', `Double' } -> `()'#}
{#fun transform          as transform        { unCairo `Cairo', `Matrix' } -> `()'#}
{#fun set_matrix         as setMatrix        { unCairo `Cairo', `Matrix' } -> `()'#}
{#fun get_matrix         as getMatrix        { unCairo `Cairo', alloca- `Matrix' peek*} -> `()'#}
{#fun identity_matrix    as identityMatrix   { unCairo `Cairo' } -> `()'#}
{#fun user_to_device     as userToDevice     { unCairo `Cairo', withFloatConv* `Double' peekFloatConv*, withFloatConv* `Double' peekFloatConv* } -> `()'#}
{#fun user_to_device_distance as userToDeviceDistance { unCairo `Cairo', withFloatConv* `Double' peekFloatConv*, withFloatConv* `Double' peekFloatConv* } -> `()'#}
{#fun device_to_user     as deviceToUser     { unCairo `Cairo', withFloatConv* `Double' peekFloatConv*, withFloatConv* `Double' peekFloatConv* } -> `()'#}
{#fun device_to_user_distance as deviceToUserDistance { unCairo `Cairo', withFloatConv* `Double' peekFloatConv*, withFloatConv* `Double' peekFloatConv* } -> `()'#}
