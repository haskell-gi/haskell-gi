{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GI.Utils.Signals
    (
     Signal,
     HasSignal(..),
     connectSignal,
     connectSignalFunPtr,
    ) where

import Foreign.Safe
import Foreign.C
import GHC.TypeLits

import GI.Utils.BasicTypes
import GI.Utils.ManagedPtr (withManagedPtr)
import GI.Utils.Utils (safeFreeFunPtrPtr)

data Signal (signal :: Symbol)

class HasSignal (signal :: Symbol) o where
    type HaskellCallbackType signal o
    type CCallbackType signal o
    mkSignalWrapper :: o -> Signal signal ->
                       HaskellCallbackType signal o ->
                           IO (FunPtr (CCallbackType signal o))
    -- TODO: This could be read directly from "signal", constructing
    -- the appropriate singleton. Do this properly once I upgrade to
    -- base 4.7.0.
    __signalName :: o -> Signal signal -> String

connectSignal :: forall s o. (HasSignal s o, GObject o, ManagedPtr o) =>
         o -> Signal s -> HaskellCallbackType s o -> Bool -> IO CULong
connectSignal object signal cb after = do
  wrapper <- mkSignalWrapper object signal cb
  connectSignalFunPtr object signalName wrapper after
    where
      signalName = __signalName object signal

-- Connecting GObjects to signals
foreign import ccall "g_signal_connect_data" g_signal_connect_data ::
    Ptr a ->                            -- instance
    CString ->                          -- detailed_signal
    FunPtr b ->                         -- c_handler
    Ptr () ->                           -- data
    FunPtr c ->                         -- destroy_data
    CUInt ->                            -- connect_flags
    IO CULong

connectSignalFunPtr :: (GObject o, ManagedPtr o) =>
                  o -> String -> FunPtr a -> Bool -> IO CULong
connectSignalFunPtr object signal fn after = do
  let flags = if after
              then 1
              else 0
  withCString signal $ \csignal -> do
    withManagedPtr object $ \objPtr ->
        g_signal_connect_data objPtr csignal fn (castFunPtrToPtr fn) safeFreeFunPtrPtr flags

