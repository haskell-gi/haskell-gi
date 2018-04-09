-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.Internal.Drawing.Paths
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see cairo/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creating paths and manipulating path data.
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo.Internal.Drawing.Paths where

{#import Graphics.Rendering.Cairo.Types#}

import Foreign
import Foreign.C
import Data.Text

import Graphics.Rendering.Cairo.Internal.Utilities (CairoString(..))

{#context lib="cairo" prefix="cairo"#}

{#fun get_current_point as getCurrentPoint { unCairo `Cairo', alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv* } -> `()'#}
{#fun new_path          as newPath         { unCairo `Cairo' } -> `()'#}
{#fun close_path        as closePath       { unCairo `Cairo' } -> `()'#}
{#fun arc               as arc             { unCairo `Cairo', `Double', `Double', `Double', `Double', `Double' } -> `()'#}
{#fun arc_negative      as arcNegative     { unCairo `Cairo', `Double', `Double', `Double', `Double', `Double' } -> `()'#}
{#fun curve_to          as curveTo         { unCairo `Cairo', `Double', `Double', `Double', `Double', `Double', `Double' } -> `()'#}
{#fun line_to           as lineTo          { unCairo `Cairo', `Double', `Double' } -> `()'#}
{#fun move_to           as moveTo          { unCairo `Cairo', `Double', `Double' } -> `()'#}
{#fun rectangle         as rectangle       { unCairo `Cairo', `Double', `Double', `Double', `Double' } -> `()'#}
textPath :: CairoString string => Cairo -> string -> IO ()
textPath c string =
    withUTFString string $ \string' ->
    {# call text_path #}
        c string'
{#fun rel_curve_to      as relCurveTo      { unCairo `Cairo', `Double', `Double', `Double', `Double', `Double', `Double' } -> `()'#}
{#fun rel_line_to       as relLineTo       { unCairo `Cairo', `Double', `Double' } -> `()'#}
{#fun rel_move_to       as relMoveTo       { unCairo `Cairo', `Double', `Double' } -> `()'#}
