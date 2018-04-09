-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.Internal.Drawing.Text
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see cairo/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Rendering text.
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo.Internal.Drawing.Text where

{#import Graphics.Rendering.Cairo.Types#}

import Graphics.Rendering.Cairo.Internal.Utilities (CairoString(..))

import Foreign
import Foreign.C

{#context lib="cairo" prefix="cairo"#}

selectFontFace :: CairoString string => Cairo -> string -> FontSlant -> FontWeight -> IO ()
selectFontFace c string slant weight =
    withUTFString string $ \string' ->
    {# call select_font_face #}
        c string' (cFromEnum slant) (cFromEnum weight)

{#fun set_font_size    as setFontSize    { unCairo `Cairo', `Double' } -> `()'#}
{#fun set_font_matrix  as setFontMatrix  { unCairo `Cairo', `Matrix' } -> `()'#}
{#fun get_font_matrix  as getFontMatrix  { unCairo `Cairo', alloca- `Matrix' peek*} -> `()'#}
{#fun set_font_options as setFontOptions { unCairo `Cairo',  withFontOptions* `FontOptions' } -> `()'#}

showText :: CairoString string => Cairo -> string -> IO ()
showText c string =
    withUTFString string $ \string' ->
    {# call show_text #}
        c string'

{#fun font_extents     as fontExtents    { unCairo `Cairo', alloca- `FontExtents' peek* } -> `()'#}

textExtents :: CairoString string => Cairo -> string -> IO TextExtents
textExtents c string =
    withUTFString string $ \string' ->
    alloca $ \result -> do
        {# call text_extents #}
            c string' result
        peek result

