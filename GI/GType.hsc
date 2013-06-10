module GI.GType
    ( GType     -- Reexport from GI.Utils.BasicTypes for convenience
    , gtypeIsA
    , gtypeIsBoxed
    ) where

#include <glib-object.h>

import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import GI.Utils.BasicTypes (GType)

foreign import ccall unsafe "g_type_is_a" g_type_is_a ::
    GType -> GType -> IO CInt

gtypeIsA :: GType -> GType -> Bool
gtypeIsA gtype is_a = (/= 0) $
    unsafePerformIO $ g_type_is_a gtype is_a

gtypeBoxed :: GType
gtypeBoxed = #const G_TYPE_BOXED

gtypeIsBoxed :: GType -> Bool
gtypeIsBoxed gtype = gtypeIsA gtype gtypeBoxed
