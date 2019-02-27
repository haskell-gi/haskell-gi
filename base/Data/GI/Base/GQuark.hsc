-- | Basic support for `GQuark`s.
module Data.GI.Base.GQuark
  ( GQuark(..)
  , gQuarkFromString
  ) where

import Data.Text (Text)
import Data.Word
import Foreign.C (CString)

import Data.GI.Base.BasicConversions (withTextCString)

#include <glib-object.h>

-- | A `GQuark`, which is simply an integer.
newtype GQuark a = GQuark (#type GQuark)

foreign import ccall g_quark_from_string :: CString -> IO (GQuark a)

-- | Construct a GQuark from the given string.
gQuarkFromString :: Text -> IO (GQuark a)
gQuarkFromString text = withTextCString text g_quark_from_string
