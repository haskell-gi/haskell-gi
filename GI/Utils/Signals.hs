{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Routines for connecting `GObject`s to signals.
module GI.Utils.Signals
    (
     Signal(..),
     HasSignal(..),
     SignalConnectMode(..),
     connectSignal,
     connectSignalFunPtr,
     on,
     after
    ) where

import Foreign.Safe
import Foreign.C
import GHC.TypeLits

import GI.Utils.BasicTypes
import GI.Utils.ManagedPtr (withManagedPtr)
import GI.Utils.Utils (safeFreeFunPtrPtr)

-- | Type tag for polymorphic signal connectors.
data Signal (signal :: Symbol) = Signal

-- | Typeclass whose members are `GObject`s with the given signal.
class (GObject o, ManagedPtr o) => HasSignal (signal :: Symbol) o where
    type HaskellCallbackType signal o
    type CCallbackType signal o
    mkSignalWrapper :: o -> Signal signal ->
                       HaskellCallbackType signal o ->
                           IO (FunPtr (CCallbackType signal o))
    -- TODO: This could be read directly from "signal", constructing
    -- the appropriate singleton. Do this properly once I upgrade to
    -- base 4.7.0.
    __signalName :: o -> Signal signal -> String

-- | Whether to connect a handler to a signal with `connectSignal` so
-- that it runs before/after the default handler for the given signal.
data SignalConnectMode = SignalConnectBefore  -- ^ Run before the default handler.
        | SignalConnectAfter -- ^ Run after the default handler.

-- | Connect a Haskell function to a signal of the given `GObject`,
-- specifying whether the handler will be called before or after the
-- default handler.
connectSignal :: forall s o. HasSignal s o =>
         SignalConnectMode -> o -> Signal s -> HaskellCallbackType s o
           -> IO CULong
connectSignal mode object signal cb = do
  wrapper <- mkSignalWrapper object signal cb
  connectSignalFunPtr object signalName wrapper mode
    where
      signalName = __signalName object signal

-- | Same as `connectSignal`, specifying from the beginning that the
-- handler is to be run before the default handler.
--
-- > on = connectSignal SignalConnectBefore
on :: HasSignal s o => o -> Signal s -> HaskellCallbackType s o -> IO CULong
on = connectSignal SignalConnectBefore

-- | Connect a signal to a handler, running the handler after the default one.
--
-- > after = connectSignal SignalConnectAfter
after :: HasSignal s o => o -> Signal s -> HaskellCallbackType s o -> IO CULong
after = connectSignal SignalConnectBefore

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
  withCString signal $ \csignal -> do
    withManagedPtr object $ \objPtr ->
        g_signal_connect_data objPtr csignal fn (castFunPtrToPtr fn) safeFreeFunPtrPtr flags

