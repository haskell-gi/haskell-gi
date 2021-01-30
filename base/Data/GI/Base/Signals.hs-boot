{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}

module Data.GI.Base.Signals (SignalInfo(..), SignalProxy, on) where

import GHC.TypeLits (Symbol)
import Data.GI.Base.BasicTypes (GObject)
import Control.Monad.IO.Class (MonadIO)
import Foreign.C (CULong)
import Data.Text (Text)
import Data.GI.Base.Overloading (ResolveSignal)

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

type role SignalProxy nominal phantom nominal
data SignalProxy object (slot::Symbol) info where

type SignalHandlerId = CULong

on :: forall object slot info m.
      (GObject object, MonadIO m, SignalInfo info, info ~ ResolveSignal slot object) =>
       object -> SignalProxy object slot info
             -> HaskellCallbackType info -> m SignalHandlerId
