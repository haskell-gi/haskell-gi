{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module SimpleExtension where

import Foreign.Ptr (Ptr)
import Control.Monad (void)

import Data.GI.Base (newObject, on, get)
import qualified GI.WebKit2WebExtension as WE

-- Make sure that the entry point of the extension is visible by the
-- glue code.
foreign export ccall initialize_simple_web_extension ::
  Ptr WE.WebExtension -> IO ()

-- | This is the entry point of our extension, called by
-- @webkit_web_extension_initialize@ in SimpleExtensionInit.c
initialize_simple_web_extension :: Ptr WE.WebExtension -> IO ()
initialize_simple_web_extension extensionPtr = do
  -- Make a managed Haskell object out of the raw C pointer.
  extension <- newObject WE.WebExtension extensionPtr

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
