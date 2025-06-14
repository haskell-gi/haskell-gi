{-# LANGUAGE OverloadedStrings #-}
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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

-- | Routines for connecting `GObject`s to signals. There are two
-- basic variants, 'on' and 'after', which correspond to
-- <https://docs.gtk.org/gobject/func.signal_connect.html g_signal_connect> and <https://docs.gtk.org/gobject/func.signal_connect_after.html g_signal_connect_after>, respectively.
--
-- Basic usage is
--
-- @ 'on' widget #signalName $ do ... @
--
-- or
--
-- @ 'after' widget #signalName $ do ... @
--
-- Note that in the Haskell bindings we represent the signal name in
-- camelCase, so a signal like <https://webkitgtk.org/reference/webkit2gtk/stable/WebKitUserContentManager.html#WebKitUserContentManager-script-message-received script-message-received> in the original API becomes <https://hackage.haskell.org/package/gi-webkit2-4.0.24/docs/GI-WebKit2-Objects-UserContentManager.html#g:16 scriptMessageReceived> in the bindings.
--
-- There are two variants of note. If you want to provide a detail
-- when connecting the signal you can use ':::', as follows:
--
-- @ 'on' widget (#scriptMessageReceived ':::' "handlerName") $ do ... @
--
-- On the other hand, if you want to connect to the "<https://hackage.haskell.org/package/gi-gobject-2.0.21/docs/GI-GObject-Objects-Object.html#g:30 notify>" signal for a property of a widget, it is recommended to use instead 'PropertyNotify', as follows:
--
-- @ 'on' widget ('PropertyNotify' #propertyName) $ do ... @
--
-- which has the advantage that it will be checked at compile time
-- that the widget does indeed have the property "@propertyName@".
module Data.GI.Base.Signals
    ( on
    , after
    , SignalProxy(..)
    , SignalConnectMode(..)
    , connectSignalFunPtr
    , disconnectSignalHandler
    , SignalHandlerId
    , SignalInfo(..)
    , GObjectNotifySignalInfo
    , SignalCodeGenError
    , resolveSignal
    , connectGObjectNotify
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Proxy (Proxy(..))
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif

import Foreign
import Foreign.C
#if !MIN_VERSION_base(4,13,0)
import Foreign.Ptr (nullPtr)
#endif

import GHC.TypeLits
import Data.Kind (Type)

import qualified Data.Text as T
import Data.Text (Text)

import Data.GI.Base.Attributes (AttrLabelProxy(..), AttrInfo(AttrLabel),
                                AttrGetType, attrGet,
                                AttrBaseTypeConstraint)
import Data.GI.Base.BasicConversions (withTextCString)
import Data.GI.Base.BasicTypes
import Data.GI.Base.GParamSpec (newGParamSpecFromPtr)
import Data.GI.Base.ManagedPtr (withManagedPtr, withTransient)
import Data.GI.Base.Overloading (ResolveSignal, ResolveAttribute,
                                 ResolvedSymbolInfo)

import GHC.OverloadedLabels (IsLabel(..))

-- | Type of a `GObject` signal handler id.
type SignalHandlerId = CULong

-- | Support for overloaded signal connectors.
data SignalProxy (object :: Type) (info :: Type) where
  -- | A basic signal name connector.
  SignalProxy :: SignalProxy o info
  -- | A signal connector annotated with a detail.
  (:::) :: forall o info. SignalProxy o info -> Text -> SignalProxy o info
  -- | A signal connector for the @notify@ signal on the given property.
  PropertyNotify :: (info ~ ResolveAttribute propName o,
                     AttrInfo info,
                     pl ~ AttrLabel info, KnownSymbol pl) =>
                    AttrLabelProxy propName ->
                    SignalProxy o GObjectNotifySignalInfo
  -- | A signal connector for the @notify@ signal on the given
  -- property, similar to `PropertyNotify`, but it passes the new
  -- value of the property to the callback for convenience.
  PropertySet :: (info ~ ResolveAttribute propName o,
                  AttrInfo info,
                  AttrBaseTypeConstraint info o,
                  b ~ AttrGetType info,
                  pl ~ AttrLabel info, KnownSymbol pl) =>
                 AttrLabelProxy propName ->
                 SignalProxy o (GObjectPropertySetSignalInfo b)

-- | Support for overloaded labels.
instance (info ~ ResolveSignal slot object) =>
    IsLabel slot (SignalProxy object info) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = SignalProxy
#else
    fromLabel _ = SignalProxy
#endif

-- | Information about an overloaded signal.
class SignalInfo (info :: Type) where
  -- | The type for the signal handler.
  type HaskellCallbackType info :: Type
  -- | Connect a Haskell function to a signal of the given `GObject`,
  -- specifying whether the handler will be called before or after the
  -- default handler. Note that the callback being passed here admits
  -- an extra initial parameter with respect to the usual Haskell
  -- callback type. This will be passed as an /implicit/ @?self@
  -- argument to the Haskell callback.
  connectSignal :: GObject o =>
                   o ->
                   (o -> HaskellCallbackType info) ->
                   SignalConnectMode ->
                   Maybe Text ->
                   IO SignalHandlerId

  -- | Optional extra debug information, for `resolveSignal` below.
  dbgSignalInfo :: Maybe ResolvedSymbolInfo
  dbgSignalInfo = Nothing

-- | Whether to connect a handler to a signal with `connectSignal` so
-- that it runs before/after the default handler for the given signal.
data SignalConnectMode = SignalConnectBefore  -- ^ Run before the default handler.
        | SignalConnectAfter -- ^ Run after the default handler.

-- | Connect a signal to a signal handler.
on :: forall object info m.
      (GObject object, MonadIO m, SignalInfo info) =>
      object -> SignalProxy object info
   -> ((?self :: object) => HaskellCallbackType info)
   -> m SignalHandlerId
on o p@(PropertySet (_ :: AttrLabelProxy propName)) cb = liftIO $ do
  let wrapped = wrapPropertySet (Proxy @propName) (Proxy @object) cb
  cb' <- mkGObjectNotifyCallback wrapped
  connectSignalFunPtr o "notify" cb' SignalConnectBefore (proxyDetail p)
on o p c =
  liftIO $ connectSignal @info o w SignalConnectBefore (proxyDetail p)
  where w :: object -> HaskellCallbackType info
        w parent = let ?self = parent in c

-- | Wrap a @b -> IO ()@ callback into a property notify callback on
-- the C side, by adding some code that reads the current value of the
-- property before invoking the callback.
wrapPropertySet :: forall info prop obj.
                   (info ~ ResolveAttribute prop obj,
                    AttrBaseTypeConstraint info obj,
                    AttrInfo info,
                    GObject obj) =>
                   Proxy (prop :: Symbol) -> Proxy obj ->
                   ((?self :: obj) => AttrGetType info -> IO ()) ->
                   Ptr obj -> Ptr GParamSpec -> Ptr () -> IO ()
wrapPropertySet _ _ cb objPtr _pspec _data =
  withTransient objPtr $ \self -> do
    val <- attrGet @(ResolveAttribute prop obj) self
    let ?self = self in cb val

-- | Connect a signal to a handler, running the handler after the default one.
after :: forall object info m.
      (GObject object, MonadIO m, SignalInfo info) =>
      object -> SignalProxy object info
      -> ((?self :: object) => HaskellCallbackType info)
      -> m SignalHandlerId
after o p c =
  liftIO $ connectSignal @info o w SignalConnectAfter (proxyDetail p)
  where w :: object -> HaskellCallbackType info
        w parent = let ?self = parent in c

-- | Given a signal proxy, determine the corresponding detail.
proxyDetail :: forall object info. SignalProxy object info -> Maybe Text
proxyDetail p = case p of
  SignalProxy -> Nothing
  (_ ::: detail) -> Just detail
  PropertyNotify (AttrLabelProxy :: AttrLabelProxy propName) ->
    Just . T.pack $ symbolVal (Proxy @(AttrLabel (ResolveAttribute propName object)))
  PropertySet (AttrLabelProxy :: AttrLabelProxy propName) ->
    Just . T.pack $ symbolVal (Proxy @(AttrLabel (ResolveAttribute propName object)))

-- Connecting GObjects to signals
foreign import ccall g_signal_connect_data ::
    Ptr a ->                            -- instance
    CString ->                          -- detailed_signal
    FunPtr b ->                         -- c_handler
    Ptr () ->                           -- data
    FunPtr c ->                         -- destroy_data
    CUInt ->                            -- connect_flags
    IO SignalHandlerId

-- Releasing the `FunPtr` for the signal handler.
foreign import ccall "& haskell_gi_release_signal_closure"
    ptr_to_release_closure :: FunPtr (Ptr () -> Ptr () -> IO ())

-- | Connect a signal to a handler, given as a `FunPtr`.
connectSignalFunPtr :: GObject o =>
                  o -> Text -> FunPtr a -> SignalConnectMode ->
                  Maybe Text -> IO SignalHandlerId
connectSignalFunPtr object signal fn mode maybeDetail = do
  let flags = case mode of
                SignalConnectAfter -> 1
                SignalConnectBefore -> 0
      signalSpec = case maybeDetail of
                     Nothing -> signal
                     Just detail -> signal <> "::" <> detail
  withTextCString signalSpec $ \csignal ->
    withManagedPtr object $ \objPtr ->
      g_signal_connect_data objPtr csignal fn nullPtr ptr_to_release_closure flags

foreign import ccall g_signal_handler_disconnect :: Ptr o -> SignalHandlerId -> IO ()

-- | Disconnect a previously connected signal.
disconnectSignalHandler :: GObject o => o -> SignalHandlerId -> IO ()
disconnectSignalHandler obj handlerId =
  withManagedPtr obj $ \objPtr ->
        g_signal_handler_disconnect objPtr handlerId

-- | Connection information for a "notify" signal indicating that a
-- specific property changed (see `PropertyNotify` for the relevant
-- constructor).
data GObjectNotifySignalInfo
instance SignalInfo GObjectNotifySignalInfo where
  type HaskellCallbackType GObjectNotifySignalInfo = GObjectNotifyCallback
  connectSignal = connectGObjectNotify

-- | Type for a `GObject` "notify" callback.
type GObjectNotifyCallback = GParamSpec -> IO ()

gobjectNotifyCallbackWrapper :: GObject o =>
  (o -> GObjectNotifyCallback) -> GObjectNotifyCallbackC o
gobjectNotifyCallbackWrapper cb selfPtr pspec _ = do
    pspec' <- newGParamSpecFromPtr pspec
    withTransient (castPtr selfPtr) $ \self -> cb self pspec'

type GObjectNotifyCallbackC o = Ptr o -> Ptr GParamSpec -> Ptr () -> IO ()

foreign import ccall "wrapper"
    mkGObjectNotifyCallback :: GObjectNotifyCallbackC o -> IO (FunPtr (GObjectNotifyCallbackC o))

-- | Connect the given notify callback for a GObject.
connectGObjectNotify :: GObject o =>
                        o -> (o -> GObjectNotifyCallback) ->
                        SignalConnectMode ->
                        Maybe Text ->
                        IO SignalHandlerId
connectGObjectNotify obj cb mode detail = do
  cb' <- mkGObjectNotifyCallback (gobjectNotifyCallbackWrapper cb)
  connectSignalFunPtr obj "notify" cb' mode detail

data GObjectPropertySetSignalInfo (b :: Type)
instance SignalInfo (GObjectPropertySetSignalInfo b) where
  type HaskellCallbackType (GObjectPropertySetSignalInfo b) = b -> IO ()
  connectSignal = undefined -- We connect these separately

-- | Generate an informative type error whenever one tries to use a
-- signal for which code generation has failed.
type family SignalCodeGenError (signalName :: Symbol) :: Type where
  SignalCodeGenError signalName = TypeError
    ('Text "The signal ‘"
     ':<>: 'Text signalName
     ':<>: 'Text "’ is not supported, because haskell-gi failed to generate appropriate bindings."
    ':$$: 'Text "Please file an issue at https://github.com/haskell-gi/haskell-gi/issues.")

-- | Return the fully qualified signal name that a given overloaded
-- signal resolves to (mostly useful for debugging).
--
-- > resolveSignal #childNotify button
resolveSignal :: forall object info. (GObject object, SignalInfo info) =>
                 object -> SignalProxy object info -> Maybe ResolvedSymbolInfo
resolveSignal _o _p = dbgSignalInfo @info
