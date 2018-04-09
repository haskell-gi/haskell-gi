-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.Internal.Drawing.Cairo
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see cairo/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- The cairo drawing context functions.
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo.Internal.Drawing.Cairo where

{#import Graphics.Rendering.Cairo.Types#}

import Foreign
import Foreign.C

{#context lib="cairo" prefix="cairo"#}

{#fun create                                 { withSurface* `Surface' } -> `Cairo' Cairo #}
{#fun reference                              { unCairo `Cairo' } -> `()' #}
{#fun destroy                                { unCairo `Cairo' } -> `()' #}
{#fun save                                   { unCairo `Cairo' } -> `()' #}
{#fun restore                                { unCairo `Cairo' } -> `()' #}
{#fun status             as status           { unCairo `Cairo' } -> `Status' cToEnum#}
{#fun get_target         as getTarget        { unCairo `Cairo' } -> `Surface' mkSurface*#}
{#fun push_group              as ^           { unCairo `Cairo' } -> `()' #}
{#fun push_group_with_content as ^           { unCairo `Cairo', cFromEnum `Content' } -> `()' #}
{#fun pop_group               as ^           { unCairo `Cairo' } -> `Pattern' Pattern #}
{#fun pop_group_to_source     as ^           { unCairo `Cairo' } -> `()' #}
{#fun set_source_rgb     as setSourceRGB     { unCairo `Cairo', `Double', `Double', `Double' } -> `()'#}
{#fun set_source_rgba    as setSourceRGBA    { unCairo `Cairo', `Double', `Double', `Double', `Double' } -> `()'#}
{#fun set_source         as setSource        { unCairo `Cairo', unPattern `Pattern' } -> `()'#}
{#fun set_source_surface as setSourceSurface { unCairo `Cairo', withSurface* `Surface', `Double', `Double' } -> `()'#}
{#fun get_source         as getSource        { unCairo `Cairo' } -> `Pattern' Pattern#}
{#fun set_antialias      as setAntialias     { unCairo `Cairo', cFromEnum `Antialias' } -> `()'#}
{#fun get_antialias      as getAntialias     { unCairo `Cairo' } -> `Antialias' cToEnum#}
setDash context xs offset = withArrayLen (map (cFloatConv) xs) $ \len ptr ->
  {#call set_dash#} context ptr (cIntConv len) (cFloatConv offset)
{#fun set_fill_rule      as setFillRule      { unCairo `Cairo', cFromEnum `FillRule' } -> `()'#}
{#fun get_fill_rule      as getFillRule      { unCairo `Cairo' } -> `FillRule' cToEnum#}
{#fun set_line_cap       as setLineCap       { unCairo `Cairo', cFromEnum `LineCap' } -> `()'#}
{#fun get_line_cap       as getLineCap       { unCairo `Cairo' } -> `LineCap' cToEnum#}
{#fun set_line_join      as setLineJoin      { unCairo `Cairo', cFromEnum `LineJoin' } -> `()'#}
{#fun get_line_join      as getLineJoin      { unCairo `Cairo' } -> `LineJoin' cToEnum#}
{#fun set_line_width     as setLineWidth     { unCairo `Cairo', `Double' } -> `()'#}
{#fun get_line_width     as getLineWidth     { unCairo `Cairo' } -> `Double'#}
{#fun set_miter_limit    as setMiterLimit    { unCairo `Cairo', `Double' } -> `()'#}
{#fun get_miter_limit    as getMiterLimit    { unCairo `Cairo' } -> `Double'#}
{#fun set_operator       as setOperator      { unCairo `Cairo', cFromEnum `Operator' } -> `()'#}
{#fun get_operator       as getOperator      { unCairo `Cairo' } -> `Operator' cToEnum#}
{#fun set_tolerance      as setTolerance     { unCairo `Cairo', `Double' } -> `()'#}
{#fun get_tolerance      as getTolerance     { unCairo `Cairo' } -> `Double'#}
{#fun clip               as clip             { unCairo `Cairo' } -> `()'#}
{#fun clip_preserve      as clipPreserve     { unCairo `Cairo' } -> `()'#}
{#fun reset_clip         as resetClip        { unCairo `Cairo' } -> `()'#}
{#fun clip_extents       as clipExtents      { unCairo `Cairo', alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv* } -> `()'#}
{#fun fill               as fill             { unCairo `Cairo' } -> `()'#}
{#fun fill_preserve      as fillPreserve     { unCairo `Cairo' } -> `()'#}
{#fun fill_extents       as fillExtents      { unCairo `Cairo', alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv* } -> `()'#}
{#fun in_fill            as inFill           { unCairo `Cairo', `Double', `Double' } -> `Bool' cToBool#}
{#fun mask               as mask             { unCairo `Cairo', unPattern `Pattern' } -> `()'#}
{#fun mask_surface       as maskSurface      { unCairo `Cairo', withSurface* `Surface', `Double', `Double' } -> `()'#}
{#fun paint              as paint            { unCairo `Cairo' } -> `()'#}
{#fun paint_with_alpha   as paintWithAlpha   { unCairo `Cairo', `Double' } -> `()'#}
{#fun stroke             as stroke           { unCairo `Cairo' } -> `()'#}
{#fun stroke_preserve    as strokePreserve   { unCairo `Cairo' } -> `()'#}
{#fun stroke_extents     as strokeExtents    { unCairo `Cairo', alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv* } -> `()'#}
{#fun in_stroke          as inStroke         { unCairo `Cairo', `Double', `Double' } -> `Bool' cToBool#}
{#fun copy_page          as copyPage         { unCairo `Cairo' } -> `()'#}
{#fun show_page          as showPage         { unCairo `Cairo' } -> `()'#}
