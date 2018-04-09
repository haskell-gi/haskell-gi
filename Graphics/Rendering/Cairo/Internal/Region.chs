-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.Internal.Region
-- Copyright   :  (c) Hamish Mackenzie 2013
-- License     :  BSD-style (see doc/COPYRIGHT)
--
-- Maintainer  :
-- Stability   :  experimental
-- Portability :  portable
--
-- Region functions.
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo.Internal.Region where

#if CAIRO_CHECK_VERSION(1,10,0)

{#import Graphics.Rendering.Cairo.Types#}

import Foreign
import Foreign.C

{#context lib="cairo" prefix="cairo"#}

regionCreateRectangles rects =
    withArrayLen rects $ \ n ptr ->
        {#call region_create_rectangles#} ptr (fromIntegral n) >>= mkRegion

{#fun region_create                 as regionCreate             {} -> `Region' mkRegion*#}
{#fun region_create_rectangle       as regionCreateRectangle    { `RectangleInt' } -> `Region' mkRegion*#}
{#fun region_copy                   as regionCopy               { withRegion* `Region' } -> `Region' mkRegion*#}
{#fun region_destroy                as regionDestroy            { withRegion* `Region' } -> `()'#}
{#fun region_reference              as regionReference          { withRegion* `Region' } -> `()'#}
{#fun region_status                 as regionStatus             { withRegion* `Region' } -> `Status' cToEnum#}
{#fun region_get_extents            as regionGetExtents         { withRegion* `Region', alloca- `RectangleInt' peek* } -> `()'#}
{#fun region_num_rectangles         as regionNumRectangles      { withRegion* `Region' } -> `Int' fromIntegral#}
{#fun region_get_rectangle          as regionGetRectangle       { withRegion* `Region', fromIntegral `Int', alloca- `RectangleInt' peek* } -> `()'#}
{#fun region_is_empty               as regionIsEmpty            { withRegion* `Region' } -> `Bool' cToBool#}
{#fun region_contains_point         as regionContainsPoint      { withRegion* `Region', fromIntegral `Int', fromIntegral `Int' } -> `Bool' cToBool#}
{#fun region_contains_rectangle     as regionContainsRectangle  { withRegion* `Region', `RectangleInt' } -> `RegionOverlap' cToEnum#}
{#fun region_equal                  as regionEqual              { withRegion* `Region', withRegion* `Region' } -> `Bool' cToBool#}
{#fun region_translate              as regionTranslate          { withRegion* `Region', fromIntegral `Int', fromIntegral `Int' } -> `()'#}
{#fun region_intersect              as regionIntersect          { withRegion* `Region', withRegion* `Region' } -> `Status' cToEnum#}
{#fun region_intersect_rectangle    as regionIntersectRectangle { withRegion* `Region', `RectangleInt' } -> `Status' cToEnum#}
{#fun region_subtract               as regionSubtract           { withRegion* `Region', withRegion* `Region' } -> `Status' cToEnum#}
{#fun region_subtract_rectangle     as regionSubtractRectangle  { withRegion* `Region', `RectangleInt' } -> `Status' cToEnum#}
{#fun region_union                  as regionUnion              { withRegion* `Region', withRegion* `Region' } -> `Status' cToEnum#}
{#fun region_union_rectangle        as regionUnionRectangle     { withRegion* `Region', `RectangleInt' } -> `Status' cToEnum#}
{#fun region_xor                    as regionXor                { withRegion* `Region', withRegion* `Region' } -> `Status' cToEnum#}
{#fun region_xor_rectangle          as regionXorRectangle       { withRegion* `Region', `RectangleInt' } -> `Status' cToEnum#}

#endif
