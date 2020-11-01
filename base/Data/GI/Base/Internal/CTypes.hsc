-- | Versions of hsc2hs older than 0.68.6 cannot deal with Haskell
-- code including promoted constructors, so isolate the required types
-- in here.
--
-- /Warning/: This module is internal, and might disappear in the future.
module Data.GI.Base.Internal.CTypes
  ( GQuark
  , C_gint
  , cgvalueSize
  , gerror_domain_offset
  , gerror_code_offset
  , gerror_message_offset
  ) where

#include <glib-object.h>

import Data.Int
import Data.Word

-- | The size in bytes of a GValue struct in C.
cgvalueSize :: Int
cgvalueSize = #size GValue

-- | The Haskell type corresponding to a GQuark on the C side.
type GQuark = #{type GQuark}

-- | The Haskell type corresponding to a gint on the C side.
type C_gint = #{type gint}

-- | The offset in bytes inside a `GError` of its @domain@ field.
gerror_domain_offset :: Int
gerror_domain_offset = #{offset GError, domain}

-- | The offset in bytes inside a `GError` of its @code@ field.
gerror_code_offset :: Int
gerror_code_offset = #{offset GError, code}

-- | The offset in bytes inside a `GError` of its @emssage@ field.
gerror_message_offset :: Int
gerror_message_offset = #{offset GError, message}
