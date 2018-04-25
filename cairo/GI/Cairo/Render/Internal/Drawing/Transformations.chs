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

#include "gi-cairo.h" 

module GI.Cairo.Render.Internal.Drawing.Transformations where

{#import GI.Cairo.Render.Types#}

import Data.GI.Base(wrapBoxed, withManagedPtr) 
import Foreign hiding (rotate)
import Foreign.C

{#context lib="cairo" prefix="cairo"#}

{#fun translate          as translate        { withManagedPtr* `Cairo', `Double', `Double' } -> `()'#}
{#fun scale              as scale            { withManagedPtr* `Cairo', `Double', `Double' } -> `()'#}
{#fun rotate             as rotate           { withManagedPtr* `Cairo', `Double' } -> `()'#}
{#fun transform          as transform        { withManagedPtr* `Cairo', with* `Matrix' } -> `()'#}
{#fun set_matrix         as setMatrix        { withManagedPtr* `Cairo', with* `Matrix' } -> `()'#}
{#fun get_matrix         as getMatrix        { withManagedPtr* `Cairo', alloca- `Matrix' peek*} -> `()'#}
{#fun identity_matrix    as identityMatrix   { withManagedPtr* `Cairo' } -> `()'#}
{#fun user_to_device     as userToDevice     { withManagedPtr* `Cairo', withFloatConv* `Double' peekFloatConv*, withFloatConv* `Double' peekFloatConv* } -> `()'#}
{#fun user_to_device_distance as userToDeviceDistance {  withManagedPtr* `Cairo', withFloatConv* `Double' peekFloatConv*, withFloatConv* `Double' peekFloatConv* } -> `()'#}
{#fun device_to_user     as deviceToUser     { withManagedPtr* `Cairo', withFloatConv* `Double' peekFloatConv*, withFloatConv* `Double' peekFloatConv* } -> `()'#}
{#fun device_to_user_distance as deviceToUserDistance { withManagedPtr* `Cairo', withFloatConv* `Double' peekFloatConv*, withFloatConv* `Double' peekFloatConv* } -> `()'#}
