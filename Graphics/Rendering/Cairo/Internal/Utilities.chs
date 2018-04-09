{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.Internal.Utilities
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see cairo/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- http://cairographics.org/manual/Support.html
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo.Internal.Utilities where

{#import Graphics.Rendering.Cairo.Types#}

import Foreign
import Foreign.C
-- TODO work around cpphs https://ghc.haskell.org/trac/ghc/ticket/13553
#if __GLASGOW_HASKELL__ >= 707 || __GLASGOW_HASKELL__ == 0
import System.IO.Unsafe (unsafePerformIO)
#endif

import Codec.Binary.UTF8.String
import Data.Char (ord, chr)
import Data.Text (Text)
import Data.ByteString (useAsCString)
import qualified Data.Text.Encoding as T (encodeUtf8)

{#context lib="cairo" prefix="cairo"#}

{#fun status_to_string    as statusToString { cFromEnum `Status' } -> `String'#}
{#fun pure version        as version        {} -> `Int'#}
{#fun pure version_string as versionString  {} -> `String'#}

class CairoString s where
    withUTFString :: s -> (CString -> IO a) -> IO a

instance CairoString [Char] where
    withUTFString = withCAString . encodeString

instance CairoString Text where
    withUTFString s = useAsCString (T.encodeUtf8 s)
