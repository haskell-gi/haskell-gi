-----------------------------------------------------------------------------
-- |
-- Module      :  GI.Cairo.Render.Internal.Drawing.Cairo
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see cairo/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- The cairo drawing context functions.
-----------------------------------------------------------------------------

#include "gi-cairo-render.h"

module GI.Cairo.Render.Internal.Drawing.Cairo where

{#import GI.Cairo.Render.Types#}

import Foreign
import Foreign.C

{#context lib="cairo" prefix="cairo"#}

{#fun create                                 { withSurface* `Surface' } -> `Cairo' #}
{#fun reference                              { `Cairo' } -> `()' #}
{#fun destroy                                { `Cairo' } -> `()' #}
{#fun save                                   { `Cairo' } -> `()' #}
{#fun restore                                { `Cairo' } -> `()' #}
{#fun status             as status           { `Cairo' } -> `Status' cToEnum#}
{#fun get_target         as getTarget        { `Cairo' } -> `Surface' mkSurface*#}
{#fun push_group              as ^           { `Cairo' } -> `()' #}
{#fun push_group_with_content as ^           { `Cairo', cFromEnum `Content' } -> `()' #}
{#fun pop_group               as ^           { `Cairo' } -> `Pattern' #}
{#fun pop_group_to_source     as ^           { `Cairo' } -> `()' #}
{#fun set_source_rgb     as setSourceRGB     { `Cairo', `Double', `Double', `Double' } -> `()'#}
{#fun set_source_rgba    as setSourceRGBA    { `Cairo', `Double', `Double', `Double', `Double' } -> `()'#}
{#fun set_source         as setSource        { `Cairo', `Pattern' } -> `()'#}
{#fun set_source_surface as setSourceSurface { `Cairo', withSurface* `Surface', `Double', `Double' } -> `()'#}
{#fun get_source         as getSource        { `Cairo' } -> `Pattern' #}
{#fun set_antialias      as setAntialias     { `Cairo', cFromEnum `Antialias' } -> `()'#}
{#fun get_antialias      as getAntialias     { `Cairo' } -> `Antialias' cToEnum#}
setDash context xs offset = withArrayLen (map (cFloatConv) xs) $ \len ptr ->
  {#call set_dash#} context ptr (cIntConv len) (cFloatConv offset)
{#fun set_fill_rule      as setFillRule      { `Cairo', cFromEnum `FillRule' } -> `()'#}
{#fun get_fill_rule      as getFillRule      { `Cairo' } -> `FillRule' cToEnum#}
{#fun set_line_cap       as setLineCap       { `Cairo', cFromEnum `LineCap' } -> `()'#}
{#fun get_line_cap       as getLineCap       { `Cairo' } -> `LineCap' cToEnum#}
{#fun set_line_join      as setLineJoin      { `Cairo', cFromEnum `LineJoin' } -> `()'#}
{#fun get_line_join      as getLineJoin      { `Cairo' } -> `LineJoin' cToEnum#}
{#fun set_line_width     as setLineWidth     { `Cairo', `Double' } -> `()'#}
{#fun get_line_width     as getLineWidth     { `Cairo' } -> `Double'#}
{#fun set_miter_limit    as setMiterLimit    { `Cairo', `Double' } -> `()'#}
{#fun get_miter_limit    as getMiterLimit    { `Cairo' } -> `Double'#}
{#fun set_operator       as setOperator      { `Cairo', cFromEnum `Operator' } -> `()'#}
{#fun get_operator       as getOperator      { `Cairo' } -> `Operator' cToEnum#}
{#fun set_tolerance      as setTolerance     { `Cairo', `Double' } -> `()'#}
{#fun get_tolerance      as getTolerance     { `Cairo' } -> `Double'#}
{#fun clip               as clip             { `Cairo' } -> `()'#}
{#fun clip_preserve      as clipPreserve     { `Cairo' } -> `()'#}
{#fun reset_clip         as resetClip        { `Cairo' } -> `()'#}
{#fun clip_extents       as clipExtents      { `Cairo', alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv* } -> `()'#}
{#fun fill               as fill             { `Cairo' } -> `()'#}
{#fun fill_preserve      as fillPreserve     { `Cairo' } -> `()'#}
{#fun fill_extents       as fillExtents      { `Cairo', alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv* } -> `()'#}
{#fun in_fill            as inFill           { `Cairo', `Double', `Double' } -> `Bool' cToBool#}
{#fun mask               as mask             { `Cairo', `Pattern' } -> `()'#}
{#fun mask_surface       as maskSurface      { `Cairo', withSurface* `Surface', `Double', `Double' } -> `()'#}
{#fun paint              as paint            { `Cairo' } -> `()'#}
{#fun paint_with_alpha   as paintWithAlpha   { `Cairo', `Double' } -> `()'#}
{#fun stroke             as stroke           { `Cairo' } -> `()'#}
{#fun stroke_preserve    as strokePreserve   { `Cairo' } -> `()'#}
{#fun stroke_extents     as strokeExtents    { `Cairo', alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv* } -> `()'#}
{#fun in_stroke          as inStroke         { `Cairo', `Double', `Double' } -> `Bool' cToBool#}
{#fun copy_page          as copyPage         { `Cairo' } -> `()'#}
{#fun show_page          as showPage         { `Cairo' } -> `()'#}
