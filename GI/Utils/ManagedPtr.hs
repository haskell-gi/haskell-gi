module GI.Utils.ManagedPtr
    ( ManagedPtr(..)
    ) where

import Foreign (Ptr)

class ManagedPtr a where
    unsafeManagedPtrGetPtr :: a -> Ptr a
    touchManagedPtr        :: a -> IO ()
