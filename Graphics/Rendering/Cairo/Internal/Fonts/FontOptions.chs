-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.Internal.Fonts.FontOptions
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see doc/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- How a font should be rendered.
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo.Internal.Fonts.FontOptions where

{#import Graphics.Rendering.Cairo.Types#}

import Foreign
import Foreign.C

{#context lib="cairo" prefix="cairo"#}

{#fun font_options_create  as fontOptionsCreate  { } -> `FontOptions' mkFontOptions*#}
{#fun font_options_copy    as fontOptionsCopy    { withFontOptions* `FontOptions' } -> `FontOptions' mkFontOptions*#}
{#fun font_options_destroy as fontOptionsDestroy { withFontOptions* `FontOptions' } -> `()'#}
{#fun font_options_status  as fontOptionsStatus  { withFontOptions* `FontOptions' } -> `Status' cToEnum#}
{#fun font_options_merge   as fontOptionsMerge   { withFontOptions* `FontOptions', withFontOptions* `FontOptions' } -> `()'#}
{#fun font_options_hash    as fontOptionsHash    { withFontOptions* `FontOptions' } -> `Int'#}
{#fun font_options_equal   as fontOptionsEqual   { withFontOptions* `FontOptions', withFontOptions* `FontOptions' } -> `Bool'#}
{#fun font_options_set_antialias      as fontOptionsSetAntialias     { withFontOptions* `FontOptions', cFromEnum `Antialias' } -> `()'#}
{#fun font_options_get_antialias      as fontOptionsGetAntialias     { withFontOptions* `FontOptions' } -> `Antialias' cToEnum#}
{#fun font_options_set_subpixel_order as fontOptionsSetSubpixelOrder { withFontOptions* `FontOptions', cFromEnum `SubpixelOrder' } -> `()'#}
{#fun font_options_get_subpixel_order as fontOptionsGetSubpixelOrder { withFontOptions* `FontOptions' } -> `SubpixelOrder' cToEnum#}
{#fun font_options_set_hint_style     as fontOptionsSetHintStyle     { withFontOptions* `FontOptions', cFromEnum `HintStyle' } -> `()'#}
{#fun font_options_get_hint_style     as fontOptionsGetHintStyle     { withFontOptions* `FontOptions' } -> `HintStyle' cToEnum#}
{#fun font_options_set_hint_metrics   as fontOptionsSetHintMetrics   { withFontOptions* `FontOptions', cFromEnum `HintMetrics' } -> `()'#}
{#fun font_options_get_hint_metrics   as fontOptionsGetHintMetrics   { withFontOptions* `FontOptions' } -> `HintMetrics' cToEnum#}
