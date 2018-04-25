-----------------------------------------------------------------------------
-- |
-- Module      :  GI.Cairo.Render.Internal.Drawing.Text
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see cairo/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Rendering text.
-----------------------------------------------------------------------------

#include "gi-cairo.h"

module GI.Cairo.Render.Internal.Drawing.Text where

{#import GI.Cairo.Render.Types#}

import GI.Cairo.Render.Internal.Utilities (CairoString(..))

import Data.GI.Base(wrapBoxed, withManagedPtr) 
import Foreign
import Foreign.C

{#context lib="cairo" prefix="cairo"#}

selectFontFace :: CairoString string => Cairo -> string -> FontSlant -> FontWeight -> IO ()
selectFontFace c string slant weight =
    withManagedPtr c $ \ctxt -> 
      withUTFString string $ \string' ->
      {# call select_font_face #}
        ctxt string' (cFromEnum slant) (cFromEnum weight)

{#fun set_font_size    as setFontSize    { withManagedPtr* `Cairo', `Double' } -> `()'#}
{#fun set_font_matrix  as setFontMatrix  { withManagedPtr* `Cairo', with* `Matrix' } -> `()'#}
{#fun get_font_matrix  as getFontMatrix  { withManagedPtr* `Cairo', alloca- `Matrix' peek*} -> `()'#}
{#fun set_font_options as setFontOptions { withManagedPtr* `Cairo',  withFontOptions* `FontOptions' } -> `()'#}

showText :: CairoString string => Cairo -> string -> IO ()
showText c string =
    withManagedPtr c $ \ctxt ->
    withUTFString string $ \string' ->
    {# call show_text #}
        ctxt string'

{#fun font_extents     as fontExtents    { withManagedPtr* `Cairo', alloca- `FontExtents' peek* } -> `()'#}

textExtents :: CairoString string => Cairo -> string -> IO TextExtents
textExtents c string =
    withManagedPtr c $ \ctxt ->
      withUTFString string $ \string' ->
      alloca $ \result -> do
         {# call text_extents #}
             ctxt string' result
         peek result
