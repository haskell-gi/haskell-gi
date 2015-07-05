{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Routines for connecting `GObject`s to signals.
module GI.Utils.Signals
    ( HasSignal(..),
      SignalConnectMode(..),
      connectSignalFunPtr,
      on,
      after,
      SignalHandlerId
    ) where

import Foreign
import Foreign.C

import GHC.TypeLits
import GHC.Exts (Constraint)

import GI.Utils.BasicTypes
import GI.Utils.ManagedPtr (withManagedPtr)
import GI.Utils.Utils (safeFreeFunPtrPtr)

#include <glib-object.h>

-- | Type of a `GObject` signal handler id.
type SignalHandlerId = #type gulong

-- | Typeclass whose members are `GObject`s with the given signal.
class (GObject o, ManagedPtr o) => HasSignal (signal :: Symbol) o where
    type HaskellCallbackType signal o
    type ConnectConstraint signal o :: Symbol -> Constraint
    -- | Connect a Haskell function to a signal of the given `GObject`,
    -- specifying whether the handler will be called before or after
    -- the default handler.
    connectSignal :: (KnownSymbol slot, ConnectConstraint signal o slot) =>
                     proxy signal slot -> o ->
                     HaskellCallbackType signal o ->
                     SignalConnectMode ->
                     IO SignalHandlerId

-- | Whether to connect a handler to a signal with `connectSignal` so
-- that it runs before/after the default handler for the given signal.
data SignalConnectMode = SignalConnectBefore  -- ^ Run before the default handler.
        | SignalConnectAfter -- ^ Run after the default handler.

-- | Same as `connectSignal`, specifying from the beginning that the
-- handler is to be run before the default handler.
--
-- > on = connectSignal SignalConnectBefore
on :: (HasSignal s o, KnownSymbol slot, ConnectConstraint s o slot) =>
      o -> proxy s slot -> HaskellCallbackType s o -> IO SignalHandlerId
on o s c = connectSignal s o c SignalConnectBefore

-- | Connect a signal to a handler, running the handler after the default one.
--
-- > after = connectSignal SignalConnectAfter
after :: (HasSignal s o, KnownSymbol slot, ConnectConstraint s o slot) =>
         o -> proxy s slot -> HaskellCallbackType s o -> IO SignalHandlerId
after o s c = connectSignal s o c SignalConnectBefore

-- Connecting GObjects to signals
foreign import ccall "g_signal_connect_data" g_signal_connect_data ::
    Ptr a ->                            -- instance
    CString ->                          -- detailed_signal
    FunPtr b ->                         -- c_handler
    Ptr () ->                           -- data
    FunPtr c ->                         -- destroy_data
    CUInt ->                            -- connect_flags
    IO SignalHandlerId

-- | Connect a signal to a handler, given as a `FunPtr`.
connectSignalFunPtr :: (GObject o, ManagedPtr o) =>
                  o -> String -> FunPtr a -> SignalConnectMode -> IO SignalHandlerId
connectSignalFunPtr object signal fn mode = do
  let flags = case mode of
                SignalConnectAfter -> 1
                SignalConnectBefore -> 0
  withCString signal $ \csignal ->
    withManagedPtr object $ \objPtr ->
        g_signal_connect_data objPtr csignal fn (castFunPtrToPtr fn) safeFreeFunPtrPtr flags
