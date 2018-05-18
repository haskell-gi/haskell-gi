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

{#fun create                                 { withManagedPtr* `Surface' } -> `ContextPtr' #}
{#fun save                                   { withManagedPtr* `Context' } -> `()' #}
{#fun restore                                { withManagedPtr* `Context' } -> `()' #}
{#fun status             as status           { withManagedPtr* `Context' } -> `Status' cToEnum#}
{#fun get_target         as getTarget        { withManagedPtr* `Context' } -> `Surface' mkSurface*#}
{#fun push_group              as ^           { withManagedPtr* `Context' } -> `()' #}
{#fun push_group_with_content as ^           { withManagedPtr* `Context', cFromEnum `Content' } -> `()' #}
{#fun pop_group               as ^           { withManagedPtr* `Context' } -> `Pattern' mkPattern*#}
{#fun pop_group_to_source     as ^           { withManagedPtr* `Context' } -> `()' #}
{#fun set_source_rgb     as setSourceRGB     { withManagedPtr* `Context', `Double', `Double', `Double' } -> `()'#}
{#fun set_source_rgba    as setSourceRGBA    { withManagedPtr* `Context', `Double', `Double', `Double', `Double' } -> `()'#}
{#fun set_source         as setSource        { withManagedPtr* `Context', withManagedPtr* `Pattern' } -> `()'#}
{#fun set_source_surface as setSourceSurface { withManagedPtr* `Context', withManagedPtr* `Surface', `Double', `Double' } -> `()'#}
{#fun get_source         as getSource        { withManagedPtr* `Context' } -> `Pattern' mkPattern* #}
{#fun set_antialias      as setAntialias     { withManagedPtr* `Context', cFromEnum `Antialias' } -> `()'#}
{#fun get_antialias      as getAntialias     { withManagedPtr* `Context' } -> `Antialias' cToEnum#}
setDash context xs offset = 
  withManagedPtr context $ \ctxt ->
    withArrayLen (map (cFloatConv) xs) $ \len ptr ->
      {#call set_dash#} ctxt ptr (cIntConv len) (cFloatConv offset)
{#fun set_fill_rule      as setFillRule      { withManagedPtr* `Context', cFromEnum `FillRule' } -> `()'#}
{#fun get_fill_rule      as getFillRule      { withManagedPtr* `Context' } -> `FillRule' cToEnum#}
{#fun set_line_cap       as setLineCap       { withManagedPtr* `Context', cFromEnum `LineCap' } -> `()'#}
{#fun get_line_cap       as getLineCap       { withManagedPtr* `Context' } -> `LineCap' cToEnum#}
{#fun set_line_join      as setLineJoin      { withManagedPtr* `Context', cFromEnum `LineJoin' } -> `()'#}
{#fun get_line_join      as getLineJoin      { withManagedPtr* `Context' } -> `LineJoin' cToEnum#}
{#fun set_line_width     as setLineWidth     { withManagedPtr* `Context', `Double' } -> `()'#}
{#fun get_line_width     as getLineWidth     { withManagedPtr* `Context' } -> `Double'#}
{#fun set_miter_limit    as setMiterLimit    { withManagedPtr* `Context', `Double' } -> `()'#}
{#fun get_miter_limit    as getMiterLimit    { withManagedPtr* `Context' } -> `Double'#}
{#fun set_operator       as setOperator      { withManagedPtr* `Context', cFromEnum `Operator' } -> `()'#}
{#fun get_operator       as getOperator      { withManagedPtr* `Context' } -> `Operator' cToEnum#}
{#fun set_tolerance      as setTolerance     { withManagedPtr* `Context', `Double' } -> `()'#}
{#fun get_tolerance      as getTolerance     { withManagedPtr* `Context' } -> `Double'#}
{#fun clip               as clip             { withManagedPtr* `Context' } -> `()'#}
{#fun clip_preserve      as clipPreserve     { withManagedPtr* `Context' } -> `()'#}
{#fun reset_clip         as resetClip        { withManagedPtr* `Context' } -> `()'#}
{#fun clip_extents       as clipExtents      { withManagedPtr* `Context', alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv* } -> `()'#}
{#fun fill               as fill             { withManagedPtr* `Context' } -> `()'#}
{#fun fill_preserve      as fillPreserve     { withManagedPtr* `Context' } -> `()'#}
{#fun fill_extents       as fillExtents      { withManagedPtr* `Context', alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv* } -> `()'#}
{#fun in_fill            as inFill           { withManagedPtr* `Context', `Double', `Double' } -> `Bool' cToBool#}
{#fun mask               as mask             { withManagedPtr* `Context', withManagedPtr* `Pattern' } -> `()'#}
{#fun mask_surface       as maskSurface      { withManagedPtr* `Context', withManagedPtr* `Surface', `Double', `Double' } -> `()'#}
{#fun paint              as paint            { withManagedPtr* `Context' } -> `()'#}
{#fun paint_with_alpha   as paintWithAlpha   { withManagedPtr* `Context', `Double' } -> `()'#}
{#fun stroke             as stroke           { withManagedPtr* `Context' } -> `()'#}
{#fun stroke_preserve    as strokePreserve   { withManagedPtr* `Context' } -> `()'#}
{#fun stroke_extents     as strokeExtents    { withManagedPtr* `Context', alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv* } -> `()'#}
{#fun in_stroke          as inStroke         { withManagedPtr* `Context', `Double', `Double' } -> `Bool' cToBool#}
{#fun copy_page          as copyPage         { withManagedPtr* `Context' } -> `()'#}
{#fun show_page          as showPage         { withManagedPtr* `Context' } -> `()'#}
