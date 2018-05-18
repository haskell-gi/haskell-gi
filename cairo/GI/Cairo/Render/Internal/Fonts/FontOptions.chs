-----------------------------------------------------------------------------
-- |
-- Module      :  GI.Cairo.Render.Internal.Fonts.FontOptions
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see doc/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- How a font should be rendered.
-----------------------------------------------------------------------------

#include "gi-cairo.h" 

module GI.Cairo.Render.Internal.Fonts.FontOptions where

{#import GI.Cairo.Render.Types#}

import Foreign
import Foreign.C
import qualified Foreign.C.Types as C2HSImp
import qualified Foreign.Ptr as C2HSImp
import qualified Foreign.Storable as C2HSImp  

import Data.GI.Base (withManagedPtr) 

{#context lib="cairo" prefix="cairo"#}

{#fun font_options_create  as fontOptionsCreate  { } -> `FontOptions' mkFontOptions*#}
{#fun font_options_copy    as fontOptionsCopy    { withManagedPtr* `FontOptions' } -> `FontOptions' mkFontOptions*#}
{#fun font_options_destroy as fontOptionsDestroy { withManagedPtr* `FontOptions' } -> `()'#}
{#fun font_options_status  as fontOptionsStatus  { withManagedPtr* `FontOptions' } -> `Status' cToEnum#}
{#fun font_options_merge   as fontOptionsMerge   { withManagedPtr* `FontOptions', withManagedPtr* `FontOptions' } -> `()'#}
{#fun font_options_hash    as fontOptionsHash    { withManagedPtr* `FontOptions' } -> `Int'#}
{#fun font_options_equal   as fontOptionsEqual   { withManagedPtr* `FontOptions', withManagedPtr* `FontOptions' } -> `Bool'#}
{#fun font_options_set_antialias      as fontOptionsSetAntialias     { withManagedPtr* `FontOptions', cFromEnum `Antialias' } -> `()'#}
{#fun font_options_get_antialias      as fontOptionsGetAntialias     { withManagedPtr* `FontOptions' } -> `Antialias' cToEnum#}
{#fun font_options_set_subpixel_order as fontOptionsSetSubpixelOrder { withManagedPtr* `FontOptions', cFromEnum `SubpixelOrder' } -> `()'#}
{#fun font_options_get_subpixel_order as fontOptionsGetSubpixelOrder { withManagedPtr* `FontOptions' } -> `SubpixelOrder' cToEnum#}
{#fun font_options_set_hint_style     as fontOptionsSetHintStyle     { withManagedPtr* `FontOptions', cFromEnum `HintStyle' } -> `()'#}
{#fun font_options_get_hint_style     as fontOptionsGetHintStyle     { withManagedPtr* `FontOptions' } -> `HintStyle' cToEnum#}
{#fun font_options_set_hint_metrics   as fontOptionsSetHintMetrics   { withManagedPtr* `FontOptions', cFromEnum `HintMetrics' } -> `()'#}
{#fun font_options_get_hint_metrics   as fontOptionsGetHintMetrics   { withManagedPtr* `FontOptions' } -> `HintMetrics' cToEnum#}
