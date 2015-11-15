{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Routines for connecting `GObject`s to signals.
module Data.GI.Base.Signals
    ( SignalConnectMode(..),
      connectSignalFunPtr,
      on,
      after,
      SignalHandlerId,
      SignalInfo(..)
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)

import Foreign
import Foreign.C

import GHC.Exts (Constraint)
import GHC.TypeLits

import Data.GI.Base.BasicTypes
import Data.GI.Base.ManagedPtr (withManagedPtr)
import Data.GI.Base.Overloading (HasSignal, ResolveSignal)
import Data.GI.Base.Utils (safeFreeFunPtrPtr)

#include <glib-object.h>

-- | Type of a `GObject` signal handler id.
type SignalHandlerId = #type gulong

-- | A proxy for passing on the signal information.
data SignalProxy (s :: *) (e :: Symbol) (c :: * -> Constraint) = SignalProxy

-- | Information about an overloaded signal.
class SignalInfo (info :: *) where
    type HaskellCallbackType info
    -- | Connect a Haskell function to a signal of the given `GObject`,
    -- specifying whether the handler will be called before or after
    -- the default handler.
    connectSignal :: (KnownSymbol extra, GObject o, constraint o) =>
                     SignalProxy info extra constraint ->
                     o ->
                     HaskellCallbackType info ->
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
on :: forall signal extra o info constraint proxy m.
      (GObject o,
       HasSignal signal o, info ~ ResolveSignal signal o, SignalInfo info,
       KnownSymbol extra, constraint o, MonadIO m) =>
      o -> proxy (signal :: Symbol) (extra :: Symbol) (constraint :: * -> Constraint)
        -> HaskellCallbackType info -> m SignalHandlerId
on o p c = liftIO $ connectSignal (resolve p) o c SignalConnectBefore
    where resolve :: proxy signal extra constraint ->
                     SignalProxy (ResolveSignal signal o) extra constraint
          resolve _ = SignalProxy

-- | Connect a signal to a handler, running the handler after the default one.
--
-- > after = connectSignal SignalConnectAfter
after :: forall signal extra o info constraint proxy m.
         (GObject o,
          HasSignal signal o, info ~ ResolveSignal signal o, SignalInfo info,
          KnownSymbol extra, constraint o, MonadIO m) =>
         o -> proxy (signal :: Symbol) (extra :: Symbol) (constraint :: * -> Constraint)
           -> HaskellCallbackType info -> m SignalHandlerId
after o p c = liftIO $ connectSignal (resolve p) o c SignalConnectAfter
    where resolve :: proxy signal extra constraint ->
                     SignalProxy (ResolveSignal signal o) extra constraint
          resolve _ = SignalProxy

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
connectSignalFunPtr :: GObject o =>
                  o -> String -> FunPtr a -> SignalConnectMode -> IO SignalHandlerId
connectSignalFunPtr object signal fn mode = do
  let flags = case mode of
                SignalConnectAfter -> 1
                SignalConnectBefore -> 0
  withCString signal $ \csignal ->
    withManagedPtr object $ \objPtr ->
        g_signal_connect_data objPtr csignal fn (castFunPtrToPtr fn) safeFreeFunPtrPtr flags
