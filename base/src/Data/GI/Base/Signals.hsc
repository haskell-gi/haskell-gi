{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Routines for connecting `GObject`s to signals.
module Data.GI.Base.Signals
    ( on
    , after
    , SignalProxy(..)
    , SignalConnectMode(..)
    , connectSignalFunPtr
    , SignalHandlerId
    , SignalInfo(..)
    , GObjectNotifySignalInfo
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Proxy (Proxy(..))

import Foreign
import Foreign.C

import GHC.TypeLits

import Data.GI.Base.Attributes (AttrLabelProxy, AttrInfo(AttrLabel))
import Data.GI.Base.BasicTypes
import Data.GI.Base.GParamSpec (newGParamSpecFromPtr)
import Data.GI.Base.ManagedPtr (withManagedPtr)
import Data.GI.Base.Overloading (ResolveSignal,
                                 IsLabelProxy(..), ResolveAttribute)
import Data.GI.Base.Utils (safeFreeFunPtrPtr)

#if MIN_VERSION_base(4,9,0)
import GHC.OverloadedLabels (IsLabel(..))
#else
import Data.GI.Base.Overloading (HasSignal)
#endif

-- | Type of a `GObject` signal handler id.
type SignalHandlerId = CULong

-- | A class that provides a constraint satisfied by every type.
class NoConstraint a
instance NoConstraint a

-- | Support for overloaded signal connectors.
data SignalProxy (object :: *) (info :: *) where
    SignalProxy :: SignalProxy o info
    PropertyNotify :: (info ~ ResolveAttribute propName o,
                       AttrInfo info,
                       pl ~ AttrLabel info) =>
                      AttrLabelProxy propName ->
                      SignalProxy o (GObjectNotifySignalInfo pl)

-- | Support for overloaded labels.
instance
#if !MIN_VERSION_base(4,9,0)
    -- This gives better error reporting in ghc < 8.0.
       (HasSignal slot object, info ~ ResolveSignal slot object)
#else
       info ~ ResolveSignal slot object
#endif
    => IsLabelProxy slot (SignalProxy object info) where
    fromLabelProxy _ = SignalProxy

#if MIN_VERSION_base(4,9,0)
instance info ~ ResolveSignal slot object =>
    IsLabel slot (SignalProxy object info) where
    fromLabel _ = SignalProxy
#endif

-- | Information about an overloaded signal.
class SignalInfo (info :: *) where
    type HaskellCallbackType info
    -- | Connect a Haskell function to a signal of the given `GObject`,
    -- specifying whether the handler will be called before or after
    -- the default handler.
    connectSignal :: GObject o =>
                     SignalProxy o info ->
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
on :: forall object info m.
      (GObject object, MonadIO m, SignalInfo info) =>
      object -> SignalProxy object info
             -> HaskellCallbackType info -> m SignalHandlerId
on o p c = liftIO $ connectSignal p o c SignalConnectBefore

-- | Connect a signal to a handler, running the handler after the default one.
--
-- > after = connectSignal SignalConnectAfter
after :: forall object info m.
      (GObject object, MonadIO m, SignalInfo info) =>
      object -> SignalProxy object info
             -> HaskellCallbackType info -> m SignalHandlerId
after o p c = liftIO $ connectSignal p o c SignalConnectAfter

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

-- | Connection information for a "notify" signal indicating that a
-- specific property changed (see `PropertyNotify` for the relevant
-- constructor).
data GObjectNotifySignalInfo (propName :: Symbol)
instance KnownSymbol propName =>
    SignalInfo (GObjectNotifySignalInfo propName) where
  type HaskellCallbackType (GObjectNotifySignalInfo propName) = GObjectNotifyCallback
  connectSignal = connectGObjectNotify (symbolVal (Proxy :: Proxy propName))

-- | Type for a `GObject` `notify` callback.
type GObjectNotifyCallback = GParamSpec -> IO ()

gobjectNotifyCallbackWrapper ::
    GObjectNotifyCallback -> Ptr () -> Ptr GParamSpec -> Ptr () -> IO ()
gobjectNotifyCallbackWrapper _cb _ pspec _ = do
    pspec' <- newGParamSpecFromPtr pspec
    _cb  pspec'

type GObjectNotifyCallbackC = Ptr () -> Ptr GParamSpec -> Ptr () -> IO ()

foreign import ccall "wrapper"
    mkGObjectNotifyCallback :: GObjectNotifyCallbackC -> IO (FunPtr GObjectNotifyCallbackC)

-- | Connect the given notify callback for a GObject.
connectGObjectNotify :: forall o i. GObject o =>
                        String ->
                        SignalProxy o (i :: *) ->
                        o -> GObjectNotifyCallback ->
                        SignalConnectMode -> IO SignalHandlerId
connectGObjectNotify propName _ obj cb mode = do
  cb' <- mkGObjectNotifyCallback (gobjectNotifyCallbackWrapper cb)
  let signalName = "notify::" ++ propName
  connectSignalFunPtr obj signalName cb' mode
