{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}

module Data.GI.Base.Signals (SignalInfo(..), SignalProxy, on) where

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
                     HaskellCallbackType info ->
                     SignalConnectMode ->
                     Maybe Text ->
                     IO SignalHandlerId

type role SignalProxy nominal nominal
data SignalProxy object info where

type SignalHandlerId = CULong

on :: forall object info m.
      (GObject object, MonadIO m, SignalInfo info) =>
       object -> SignalProxy object info
             -> HaskellCallbackType info -> m SignalHandlerId
