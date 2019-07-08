-- | The Haskell Prelude exports a number of symbols that can easily
-- collide with functions appearing in bindings. The generated code
-- requires just a small subset of the functions in the Prelude,
-- together with some of the functionality in Data.GI.Base, we
-- reexport this explicitly here.
module Data.GI.Base.ShortPrelude
    ( module Data.Char
    , module Data.Int
    , module Data.Word
    , module Data.ByteString.Char8
    , module Foreign.C
    , module Foreign.Ptr
    , module Foreign.ForeignPtr
    , module Foreign.ForeignPtr.Unsafe
    , module Foreign.Storable
    , module Control.Applicative
    , module Control.Exception
    , module Control.Monad.IO.Class

    , module Data.GI.Base.Attributes
    , module Data.GI.Base.BasicTypes
    , module Data.GI.Base.BasicConversions
    , module Data.GI.Base.GClosure
    , module Data.GI.Base.Constructible
    , module Data.GI.Base.GError
    , module Data.GI.Base.GHashTable
    , module Data.GI.Base.GParamSpec
    , module Data.GI.Base.GObject
    , module Data.GI.Base.GVariant
    , module Data.GI.Base.GValue
    , module Data.GI.Base.ManagedPtr
    , module Data.GI.Base.Signals
    , module Data.GI.Base.Utils

    , module GHC.TypeLits

    , Enum(fromEnum, toEnum)
    , Show(..)
    , Eq(..)
    , IO
    , Monad(..)
    , Maybe(..)
    , (.)
    , ($)
    , (++)
    , (=<<)
    , (>=>)
    , Bool()
    , Float
    , Double
    , undefined
    , error
    , map
    , length
    , mapM
    , mapM_
    , when
    , fromIntegral
    , realToFrac
    ) where

import Control.Monad (when, (>=>))
import Data.Char (Char, ord, chr)
import Data.Int (Int, Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.ByteString.Char8 (ByteString)
import Foreign.C (CInt(..), CUInt(..), CFloat(..), CDouble(..), CString, CIntPtr(..), CUIntPtr(..), CLong(..), CULong(..))
import Foreign.Ptr (Ptr, plusPtr, FunPtr, nullPtr,
                    castFunPtrToPtr, castPtrToFunPtr)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Storable (peek, poke, sizeOf)
import Control.Applicative ((<$>))
import Control.Exception (onException)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.GI.Base.Attributes hiding (get, set)
import Data.GI.Base.BasicTypes
import Data.GI.Base.BasicConversions
import Data.GI.Base.Constructible
import Data.GI.Base.GClosure (GClosure)
import Data.GI.Base.GError
import Data.GI.Base.GHashTable
import Data.GI.Base.GObject
import Data.GI.Base.GParamSpec
import Data.GI.Base.GVariant
import Data.GI.Base.GValue
import Data.GI.Base.ManagedPtr
import Data.GI.Base.Signals (SignalConnectMode(..), connectSignalFunPtr, SignalHandlerId, SignalInfo(..), GObjectNotifySignalInfo)
import Data.GI.Base.Utils

import GHC.TypeLits (Symbol)
