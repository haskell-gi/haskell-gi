{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module SimpleExtension where

import Control.Monad (void, forM)
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)

import Data.GI.Base (newObject, on, get, GVariant, unsafeCastTo)
import Data.GI.Base.GVariant (newGVariantFromPtr, fromGVariant, toGVariant,
                              GVariantSinglet(..))
import qualified GI.Gio as Gio
import qualified GI.WebKit2WebExtension as WE

import DBusHelpers (registerDBusServer, execDBusMethod)
import DBusServersInfo (extensionServerInfo, browserServerInfo)

-- Make sure that the entry point of the extension is visible by the
-- glue code.
foreign export ccall initialize_simple_web_extension_with_user_data ::
  Ptr WE.WebExtension -> Ptr GVariant -> IO ()

highlightLinks :: WE.WebPage -> IO Word32
highlightLinks page = do
      maybeDom <- #getDomDocument page
      links <- case maybeDom of
        Just dom -> do
          selector <- #querySelectorAll dom "a"
          l <- selector `get` #length
          forM [0 .. ((fromIntegral l :: Int) - 1)] $ \i -> do
            maybeNode <- #item selector (fromIntegral i)
            case maybeNode of
              Just node -> do
                nodeType <- node `get` #nodeType
                case fromIntegral nodeType of
                  WE.DOM_NODE_ELEMENT_NODE -> do
                    el <- unsafeCastTo WE.DOMElement node
                    style <- #getStyle el
                    #setProperty style "background-color" "#ffc" ""
                    #setProperty style "color" "#000" ""
                    #setProperty style "border" "1px solid #000" ""
                    #setProperty style "box-shadow" "0 4px 8px 0 rgba(1.0, 1.0, 0, 0.2), 0 6px 20px 0 rgba(1.0, 1.0, 0, 0.19)" ""
                    label <- el `get` #innerHtml
                    return $ Just label
                  _ -> return Nothing
              Nothing -> return Nothing
        Nothing -> return []
      return $ fromIntegral $ length $ catMaybes links

invokeExtensionMethod :: WE.WebPage -> Text -> GVariant -> IO (Maybe GVariant)
invokeExtensionMethod page methodName parameters =
  case methodName of
    "highlightLinks" -> fromGVariant parameters >>= \case
      Just () -> do
        nlinks <- highlightLinks page
        Just <$> toGVariant (GVariantSinglet nlinks)
      Nothing -> do
        putStrLn "Could not parse parameters!"
        return Nothing
    _ -> do
      putStrLn $ "Unknown method: " <> show methodName
      return Nothing

-- | Notify the browser that we are around.
extensionActivated :: IO ()
extensionActivated = do
  sessionBus <- Gio.busGetSync Gio.BusTypeSession Gio.noCancellable
  execDBusMethod sessionBus browserServerInfo "extensionActivated" Nothing Nothing

-- | This is the entry point of our extension, called by
-- @webkit_web_extension_initialize@ in SimpleExtensionInit.c
initialize_simple_web_extension_with_user_data ::
  Ptr WE.WebExtension -> Ptr GVariant -> IO ()
initialize_simple_web_extension_with_user_data extensionPtr dataPtr = do
  -- Make a managed Haskell object out of the raw C pointer.
  extension <- newObject WE.WebExtension extensionPtr
  userData <- newGVariantFromPtr dataPtr
  fromGVariant userData >>= \case
    Just (str, count) ->
      putStrLn $ "The string was " <> show (str :: Text) <>
                 ", and the count was " <> show (count :: Int64)
    Nothing -> putStrLn "Could not decode user data!"

  -- From now on the @extension@ object can be used normally from
  -- Haskell code.

  void $ on extension #pageCreated $ \page -> do
    registerDBusServer extensionServerInfo (invokeExtensionMethod page)
                       (extensionActivated)
