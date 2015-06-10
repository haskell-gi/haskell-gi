{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Routines for connecting `GObject`s to signals.
module GI.Utils.Signals
    (
     HasSignal(..),
     SignalConnectMode(..),
     connectSignalFunPtr,
     on,
     after
    ) where

import Foreign
import Foreign.C
import GHC.TypeLits

import GI.Utils.BasicTypes
import GI.Utils.ManagedPtr (withManagedPtr)
import GI.Utils.Utils (safeFreeFunPtrPtr)

-- | Typeclass whose members are `GObject`s with the given signal.
class (GObject o, ManagedPtr o) => HasSignal (signal :: Symbol) o where
    type HaskellCallbackType signal o
    -- | Connect a Haskell function to a signal of the given `GObject`,
    -- specifying whether the handler will be called before or after
    -- the default handler.
    connectSignal :: proxy signal -> o ->
                     HaskellCallbackType signal o ->
                     SignalConnectMode ->
                     IO CULong

-- | Whether to connect a handler to a signal with `connectSignal` so
-- that it runs before/after the default handler for the given signal.
data SignalConnectMode = SignalConnectBefore  -- ^ Run before the default handler.
        | SignalConnectAfter -- ^ Run after the default handler.

-- | Same as `connectSignal`, specifying from the beginning that the
-- handler is to be run before the default handler.
--
-- > on = connectSignal SignalConnectBefore
on :: HasSignal s o => o -> proxy s -> HaskellCallbackType s o -> IO CULong
on o s c = connectSignal s o c SignalConnectBefore

-- | Connect a signal to a handler, running the handler after the default one.
--
-- > after = connectSignal SignalConnectAfter
after :: HasSignal s o => o -> proxy s -> HaskellCallbackType s o -> IO CULong
after o s c = connectSignal s o c SignalConnectBefore

-- Connecting GObjects to signals
foreign import ccall "g_signal_connect_data" g_signal_connect_data ::
    Ptr a ->                            -- instance
    CString ->                          -- detailed_signal
    FunPtr b ->                         -- c_handler
    Ptr () ->                           -- data
    FunPtr c ->                         -- destroy_data
    CUInt ->                            -- connect_flags
    IO CULong

-- | Connect a signal to a handler, given as a `FunPtr`.
connectSignalFunPtr :: (GObject o, ManagedPtr o) =>
                  o -> String -> FunPtr a -> SignalConnectMode -> IO CULong
connectSignalFunPtr object signal fn mode = do
  let flags = case mode of
                SignalConnectAfter -> 1
                SignalConnectBefore -> 0
  withCString signal $ \csignal ->
    withManagedPtr object $ \objPtr ->
        g_signal_connect_data objPtr csignal fn (castFunPtrToPtr fn) safeFreeFunPtrPtr flags
