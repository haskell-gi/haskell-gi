{-# LANGUAGE OverloadedStrings #-}

-- | Definitions of the D-Bus method servers to be installed by the
-- browser and the extension.
module DBusServersInfo
  ( extensionServerInfo
  , browserServerInfo
  ) where

import DBusHelpers (DBusServerInfo(..))

-- | Definition of the method server installed by the extension.
extensionServerInfo :: DBusServerInfo
extensionServerInfo = DBusServerInfo {
  serverBusName         = "haskellGI.test.simpleExtension",
  serverXMLInfo         = xml,
  serverInterfaceName   = "test.simpleExtension",
  serverObjectPath      = "/test/simpleExtension"
  } where
  xml = "<node>" <>
        "  <interface name='test.simpleExtension'>" <>
        "    <method name='highlightLinks'>" <>
        "      <arg type='u' name='nlinks' direction='out'/>" <>
        "    </method>" <>
        "  </interface>" <>
        "</node>"

-- | Definition of the D-Bus server to be installed by the browser.
browserServerInfo :: DBusServerInfo
browserServerInfo = DBusServerInfo {
  serverBusName         = "haskellGI.test.simpleBrowser",
  serverXMLInfo         = xml,
  serverInterfaceName   = "test.simpleBrowser",
  serverObjectPath      = "/test/simpleBrowser"
  } where
  xml = "<node>" <>
        "  <interface name='test.simpleBrowser'>" <>
        "    <method name='extensionActivated'>" <>
        "    </method>" <>
        "  </interface>" <>
        "</node>"
