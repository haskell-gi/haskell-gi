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

#include "gi-cairo.h"

module GI.Cairo.Render.Internal.Drawing.Cairo where

{#import GI.Cairo.Render.Types#}

import Data.GI.Base(wrapBoxed, withManagedPtr)
import Foreign
import Foreign.C

{#context lib="cairo" prefix="cairo"#}

{#fun create                                 { withSurface* `Surface' } -> `CairoPtr' #}
{#fun save                                   { withManagedPtr* `Cairo' } -> `()' #}
{#fun restore                                { withManagedPtr* `Cairo' } -> `()' #}
{#fun status             as status           { withManagedPtr* `Cairo' } -> `Status' cToEnum#}
{#fun get_target         as getTarget        { withManagedPtr* `Cairo' } -> `Surface' mkSurface*#}
{#fun push_group              as ^           { withManagedPtr* `Cairo' } -> `()' #}
{#fun push_group_with_content as ^           { withManagedPtr* `Cairo', cFromEnum `Content' } -> `()' #}
{#fun pop_group               as ^           { withManagedPtr* `Cairo' } -> `Pattern' #}
{#fun pop_group_to_source     as ^           { withManagedPtr* `Cairo' } -> `()' #}
{#fun set_source_rgb     as setSourceRGB     { withManagedPtr* `Cairo', `Double', `Double', `Double' } -> `()'#}
{#fun set_source_rgba    as setSourceRGBA    { withManagedPtr* `Cairo', `Double', `Double', `Double', `Double' } -> `()'#}
{#fun set_source         as setSource        { withManagedPtr* `Cairo', `Pattern' } -> `()'#}
{#fun set_source_surface as setSourceSurface { withManagedPtr* `Cairo', withSurface* `Surface', `Double', `Double' } -> `()'#}
{#fun get_source         as getSource        { withManagedPtr* `Cairo' } -> `Pattern' #}
{#fun set_antialias      as setAntialias     { withManagedPtr* `Cairo', cFromEnum `Antialias' } -> `()'#}
{#fun get_antialias      as getAntialias     { withManagedPtr* `Cairo' } -> `Antialias' cToEnum#}
setDash context xs offset = 
  withManagedPtr context $ \ctxt ->
    withArrayLen (map (cFloatConv) xs) $ \len ptr ->
      {#call set_dash#} ctxt ptr (cIntConv len) (cFloatConv offset)
{#fun set_fill_rule      as setFillRule      { withManagedPtr* `Cairo', cFromEnum `FillRule' } -> `()'#}
{#fun get_fill_rule      as getFillRule      { withManagedPtr* `Cairo' } -> `FillRule' cToEnum#}
{#fun set_line_cap       as setLineCap       { withManagedPtr* `Cairo', cFromEnum `LineCap' } -> `()'#}
{#fun get_line_cap       as getLineCap       { withManagedPtr* `Cairo' } -> `LineCap' cToEnum#}
{#fun set_line_join      as setLineJoin      { withManagedPtr* `Cairo', cFromEnum `LineJoin' } -> `()'#}
{#fun get_line_join      as getLineJoin      { withManagedPtr* `Cairo' } -> `LineJoin' cToEnum#}
{#fun set_line_width     as setLineWidth     { withManagedPtr* `Cairo', `Double' } -> `()'#}
{#fun get_line_width     as getLineWidth     { withManagedPtr* `Cairo' } -> `Double'#}
{#fun set_miter_limit    as setMiterLimit    { withManagedPtr* `Cairo', `Double' } -> `()'#}
{#fun get_miter_limit    as getMiterLimit    { withManagedPtr* `Cairo' } -> `Double'#}
{#fun set_operator       as setOperator      { withManagedPtr* `Cairo', cFromEnum `Operator' } -> `()'#}
{#fun get_operator       as getOperator      { withManagedPtr* `Cairo' } -> `Operator' cToEnum#}
{#fun set_tolerance      as setTolerance     { withManagedPtr* `Cairo', `Double' } -> `()'#}
{#fun get_tolerance      as getTolerance     { withManagedPtr* `Cairo' } -> `Double'#}
{#fun clip               as clip             { withManagedPtr* `Cairo' } -> `()'#}
{#fun clip_preserve      as clipPreserve     { withManagedPtr* `Cairo' } -> `()'#}
{#fun reset_clip         as resetClip        { withManagedPtr* `Cairo' } -> `()'#}
{#fun clip_extents       as clipExtents      { withManagedPtr* `Cairo', alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv* } -> `()'#}
{#fun fill               as fill             { withManagedPtr* `Cairo' } -> `()'#}
{#fun fill_preserve      as fillPreserve     { withManagedPtr* `Cairo' } -> `()'#}
{#fun fill_extents       as fillExtents      { withManagedPtr* `Cairo', alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv* } -> `()'#}
{#fun in_fill            as inFill           { withManagedPtr* `Cairo', `Double', `Double' } -> `Bool' cToBool#}
{#fun mask               as mask             { withManagedPtr* `Cairo', `Pattern' } -> `()'#}
{#fun mask_surface       as maskSurface      { withManagedPtr* `Cairo', withSurface* `Surface', `Double', `Double' } -> `()'#}
{#fun paint              as paint            { withManagedPtr* `Cairo' } -> `()'#}
{#fun paint_with_alpha   as paintWithAlpha   { withManagedPtr* `Cairo', `Double' } -> `()'#}
{#fun stroke             as stroke           { withManagedPtr* `Cairo' } -> `()'#}
{#fun stroke_preserve    as strokePreserve   { withManagedPtr* `Cairo' } -> `()'#}
{#fun stroke_extents     as strokeExtents    { withManagedPtr* `Cairo', alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv* } -> `()'#}
{#fun in_stroke          as inStroke         { withManagedPtr* `Cairo', `Double', `Double' } -> `Bool' cToBool#}
{#fun copy_page          as copyPage         { withManagedPtr* `Cairo' } -> `()'#}
{#fun show_page          as showPage         { withManagedPtr* `Cairo' } -> `()'#}
