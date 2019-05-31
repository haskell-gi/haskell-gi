{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module SimpleExtension where

import Control.Monad (void)
import Data.Int (Int64)
import Data.Monoid ((<>))
import Data.Text (Text)
import Foreign.Ptr (Ptr)

import Data.GI.Base (newObject, on, get, GVariant)
import Data.GI.Base.GVariant (newGVariantFromPtr, fromGVariant)
import qualified GI.WebKit2WebExtension as WE

-- Make sure that the entry point of the extension is visible by the
-- glue code.
foreign export ccall initialize_simple_web_extension_with_user_data ::
  Ptr WE.WebExtension -> Ptr GVariant -> IO ()

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
    void $ on page #documentLoaded $ do
      uri <- page `get` #uri
      maybeDom <- #getDomDocument page
      maybeTitle <- case maybeDom of
        Just dom -> dom `get` #title
        Nothing -> return Nothing
      putStrLn $ "Loaded " <> show uri <> " with title "
        <> show maybeTitle <> "."
