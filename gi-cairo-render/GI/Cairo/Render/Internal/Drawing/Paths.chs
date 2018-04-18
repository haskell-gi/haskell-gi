-----------------------------------------------------------------------------
-- |
-- Module      :  GI.Cairo.Render.Internal.Drawing.Paths
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see cairo/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creating paths and manipulating path data.
-----------------------------------------------------------------------------

#include "gi-cairo-render.h" 

module GI.Cairo.Render.Internal.Drawing.Paths where

{#import GI.Cairo.Render.Types#}

import Data.GI.Base(wrapBoxed, withManagedPtr) 
import Foreign
import Foreign.C
import Data.Text

import GI.Cairo.Render.Internal.Utilities (CairoString(..))

{#context lib="cairo" prefix="cairo"#}

{#fun get_current_point as getCurrentPoint { withManagedPtr* `Cairo', alloca- `Double' peekFloatConv*, alloca- `Double' peekFloatConv* } -> `()'#}
{#fun new_path          as newPath         { withManagedPtr* `Cairo' } -> `()'#}
{#fun close_path        as closePath       { withManagedPtr* `Cairo' } -> `()'#}
{#fun arc               as arc             { withManagedPtr* `Cairo', `Double', `Double', `Double', `Double', `Double' } -> `()'#}
{#fun arc_negative      as arcNegative     { withManagedPtr* `Cairo', `Double', `Double', `Double', `Double', `Double' } -> `()'#}
{#fun curve_to          as curveTo         { withManagedPtr* `Cairo', `Double', `Double', `Double', `Double', `Double', `Double' } -> `()'#}
{#fun line_to           as lineTo          { withManagedPtr* `Cairo', `Double', `Double' } -> `()'#}
{#fun move_to           as moveTo          { withManagedPtr* `Cairo', `Double', `Double' } -> `()'#}
{#fun rectangle         as rectangle       { withManagedPtr* `Cairo', `Double', `Double', `Double', `Double' } -> `()'#}
textPath :: CairoString string => Cairo -> string -> IO ()
textPath c string =
    withManagedPtr c $ \ctxt ->
      withUTFString string $ \string' ->
      {# call text_path #} ctxt string'
{#fun rel_curve_to      as relCurveTo      { withManagedPtr* `Cairo', `Double', `Double', `Double', `Double', `Double', `Double' } -> `()'#}
{#fun rel_line_to       as relLineTo       { withManagedPtr* `Cairo', `Double', `Double' } -> `()'#}
{#fun rel_move_to       as relMoveTo       { withManagedPtr* `Cairo', `Double', `Double' } -> `()'#}
