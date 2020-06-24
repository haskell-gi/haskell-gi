{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- A simple DBus server. See examples/WebKit/DBusHelpers.hs for a more
-- advanced example. To run the "factor" method in the server you can use
-- https://wiki.gnome.org/Apps/DFeet.

import qualified GI.GLib as GLib
import qualified GI.Gio as Gio

import Data.GI.Base
import Data.Text (Text)
import Data.Word (Word32)

factor :: Word32 -> [Word32]
factor n = case factors of
             [] -> [n]
             _  -> factors ++ factor (n `div` (head factors))
    where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]


methodCall :: Gio.DBusConnection -> Text -> Text -> Text -> Text ->
              GVariant -> Gio.DBusMethodInvocation -> IO ()
methodCall _connection _sender _objectPath _interfaceName methodName parameters invocation = do
  case methodName of
      "factor" -> fromGVariant parameters >>= \case
                  Just (GVariantSinglet n) -> do
                      result <- toGVariant (GVariantSinglet (factor n))
                      putStrLn ("Factoring " <> show (n :: Word32))
                      #returnValue invocation (Just result)
                  Nothing -> do
                    putStrLn "Could not parse parameters!"
                    #returnValue invocation Nothing
      _ -> #returnValue invocation Nothing

onBusAcquired :: Gio.DBusConnection -> Text -> IO ()
onBusAcquired connection _ = do
  putStrLn "Bus acquired!"
  info <- Gio.dBusNodeInfoNewForXml introspection_xml
  interface <- #lookupInterface info "test.haskellGI"
  putStrLn "Introspection data parsed succesfully!"
  methodCallPtr <- Gio.genClosure_DBusInterfaceMethodCallFunc methodCall
  r <- #registerObject connection "/test/haskellGI" interface
       (Just methodCallPtr) Nothing Nothing
  putStrLn ("Registered! " <> show r)
  where
    introspection_xml = "<node>" <>
                       "  <interface name='test.haskellGI'>" <>
                       "    <method name='factor'>" <>
                       "      <arg type='u' name='n' direction='in'/>" <>
                       "      <arg type='au' name='factors' direction='out'/>" <>
                       "    </method>" <>
                       "  </interface>" <>
                       "</node>"

ownBus :: IO Word32
ownBus = do
  putStrLn "Owning name"
  ba <- Gio.genClosure_BusAcquiredCallback onBusAcquired
  Gio.busOwnName Gio.BusTypeSession "test.haskellGI.factor"
     [] (Just ba) Nothing Nothing

main :: IO ()
main = do
  mainloop <- GLib.mainLoopNew Nothing False

  ownerId <- ownBus

  #run mainloop

  Gio.busUnownName ownerId
