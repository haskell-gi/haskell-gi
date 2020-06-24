{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

-- | Helper functions for registering D-Bus servers fomr Haskell.  See
-- the [D-Bus
-- specification](https://dbus.freedesktop.org/doc/dbus-specification.html)
-- for technical details, and
-- [D-Feet](https://wiki.gnome.org/Apps/DFeet) for a useful D-Bus
-- debugger.
module DBusHelpers
  ( DBusServerInfo(..)
  , registerDBusServer
  , execDBusMethod
  ) where

import Data.Text (Text)

import Data.GI.Base (GVariant)

import qualified GI.Gio as Gio

-- | Type for the callback that invokes methods in the server
type ServerMethodCall = Text            -- ^ Method name
                      -> GVariant       -- ^ Params
                      -> IO (Maybe GVariant) -- ^ Result

-- | Callback for when the bus has been acquired
type ServerBusAcquired = IO ()

-- | Information about the server to be installed.
data DBusServerInfo = DBusServerInfo {
  -- | Bus name for the server, typically something like @org.haskell.AppName@.
  serverBusName         :: Text,


  -- | Path in the bus for the interface containing the
  -- methods. Typically something like @/org/haskell/AppName/Actions@
  serverObjectPath      :: Text,

  -- | Introspection info for the object containing the methods. See
  -- <https://dbus.freedesktop.org/doc/dbus-specification.html#introspection-format>
  -- for the specification.
  serverXMLInfo         :: Text,

  -- | Name of the interface containing the methods, typically
  -- something like @org.haskell.AppName.RemoteActions@.
  serverInterfaceName   :: Text
  }

-- | Invoke the `ServerMethodCall` callback with the given parameters.
dispatchMethod :: ServerMethodCall
               -> Gio.DBusConnection
               -> Text -> Text -> Text -> Text
               -> GVariant -> Gio.DBusMethodInvocation -> IO ()
dispatchMethod methodCall _connection _sender _objectPath _interfaceName
               methodName parameters invocation = do
  result <- methodCall methodName parameters
  #returnValue invocation result

-- | Attach the interface with the methods to the bus, and invoke the
-- `ServerBusAcquired` callback.
onBusAcquired :: DBusServerInfo -> ServerMethodCall -> ServerBusAcquired ->
                 Gio.DBusConnection -> Text -> IO ()
onBusAcquired info methodCall acquired connection _ = do
  putStrLn "Bus acquired!"
  nodeInfo <- Gio.dBusNodeInfoNewForXml (serverXMLInfo info)
  interface <- #lookupInterface nodeInfo (serverInterfaceName info)
  putStrLn "Introspection data parsed succesfully!"
  methodCallPtr <- Gio.genClosure_DBusInterfaceMethodCallFunc
                   (dispatchMethod methodCall)

  r <- #registerObject connection (serverObjectPath info) interface
       (Just methodCallPtr) Nothing Nothing
  acquired
  putStrLn ("Registered! " <> show r)

-- | Register the given server.
registerDBusServer :: DBusServerInfo -> ServerMethodCall -> ServerBusAcquired
                   -> IO ()
registerDBusServer info methodCall acquired = do
  ba <- Just <$> Gio.genClosure_BusAcquiredCallback
                 (onBusAcquired info methodCall acquired)
  _ <- Gio.busOwnName Gio.BusTypeSession (serverBusName info)
       [] ba Nothing Nothing
  return ()

-- | Type for the callback invoked when a D-Bus method call returns
-- with a value.
type DBusResultReady = GVariant -> IO ()

-- | Run the given method (asynchronously) on the given D-Bus server.
execDBusMethod :: Gio.DBusConnection -- ^ Connection to the bus
               -> DBusServerInfo -- ^ Info for the server.
               -> Text -- ^ Name of the method to invoke.
               -> Maybe GVariant -- ^ Params (or `Nothing`).
               -> Maybe DBusResultReady -- ^ A callback to be invoked
                                        -- when the result is
                                        -- received, or 'Nothing'.
               -> IO ()
execDBusMethod connection info methodName params resultCallback =
  #call connection (Just $ serverBusName info) (serverObjectPath info)
        (serverInterfaceName info)
        methodName params Nothing []
        (-1) (Nothing @Gio.Cancellable)
        (wrapResultCB <$> resultCallback)

  where wrapResultCB :: DBusResultReady -> Gio.AsyncReadyCallback
        wrapResultCB cb _maybeObj asyncResult = do
          result <- #callFinish connection asyncResult
          cb result
