{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

module Data.GI.Base.Signals (SignalInfo(..), SignalProxy, on, after) where

import Data.GI.Base.Overloading (ResolvedSymbolInfo)
import Data.GI.Base.BasicTypes (GObject)
import Control.Monad.IO.Class (MonadIO)
import Foreign.C (CULong)
import Data.Text (Text)

data SignalConnectMode = SignalConnectBefore
        | SignalConnectAfter

class SignalInfo info where
  type HaskellCallbackType info
  connectSignal :: GObject o =>
                     o ->
                     (o -> HaskellCallbackType info) ->
                     SignalConnectMode ->
                     Maybe Text ->
                     IO SignalHandlerId
  dbgSignalInfo :: Maybe ResolvedSymbolInfo
  dbgSignalInfo = Nothing

type role SignalProxy nominal nominal
data SignalProxy object info where

type SignalHandlerId = CULong

on :: forall object info m.
      (GObject object, MonadIO m, SignalInfo info) =>
       object -> SignalProxy object info
             -> ((?self :: object) => HaskellCallbackType info) -> m SignalHandlerId

after :: forall object info m.
      (GObject object, MonadIO m, SignalInfo info) =>
       object -> SignalProxy object info
             -> ((?self :: object) => HaskellCallbackType info) -> m SignalHandlerId
