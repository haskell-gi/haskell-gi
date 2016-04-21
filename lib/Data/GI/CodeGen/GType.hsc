module Data.GI.CodeGen.GType
    ( GType     -- Reexport from Data.GI.Base.BasicTypes for convenience
    , gtypeIsA
    , gtypeIsBoxed
    ) where

#include <glib-object.h>

import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import Data.GI.Base.BasicTypes (CGType, GType(..))

foreign import ccall unsafe "g_type_is_a" g_type_is_a ::
    CGType -> CGType -> IO CInt

gtypeIsA :: GType -> GType -> Bool
gtypeIsA (GType gtype) (GType is_a) = (/= 0) $
    unsafePerformIO $ g_type_is_a gtype is_a

gtypeBoxed :: GType
gtypeBoxed = GType #const G_TYPE_BOXED

gtypeIsBoxed :: GType -> Bool
gtypeIsBoxed gtype = gtypeIsA gtype gtypeBoxed
