{-# LANGUAGE CPP #-}
{-|
A module aimed at making working with GtkBuilder easier.
It's meant to be used like this (requires OverloadedStrings):

> buildUI :: BuildFn ()
> buildUI = do
>     mainWin <- buildMainWin
>     buildAboutDialog
>
>     widgetShowAll mainWin
>
> buildMainWin :: BuildFn ApplicationWindow
> buildMainWin = do
>     buildHeaderBar
>     buildMenuBar
>     buildMainArea
>
>     mainWin <- getObject' ApplicationWindow "mainWin"
>     on mainWin Destroy mainQuit
>     return mainWin
>
> buildAboutDialog :: BuildFn AboutDialog
> ...
>
> buildHeaderBar :: BuildFn HeaderBar
> ...
>
> buildMenuBar :: BuildFn MenuBar
> ...
>
> buildMainArea :: BuildFn Grid
> ...
-}
module Data.GI.Gtk.BuildFn
    ( BuildFn
    , buildWithBuilder
    , getObject
    ) where

import           Prelude ()
import           Prelude.Compat
import           Control.Monad.Reader (ReaderT, runReaderT, ask, MonadIO, liftIO)
import           Data.GI.Base (GObject, castTo)
#if !MIN_VERSION_haskell_gi_base(0,20,1)
import           Data.GI.Base.BasicTypes (nullToNothing)
#endif
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           Foreign.ForeignPtr (ForeignPtr)

import           GI.Gtk hiding (main)

type BuildFn a = ReaderT Builder IO a

buildWithBuilder :: MonadIO m => BuildFn a -> Builder -> m a
buildWithBuilder fn builder = liftIO $ runReaderT fn builder

getObject :: GObject a => (ManagedPtr a -> a) -> T.Text -> BuildFn a
getObject ctor name = do
    builder <- ask
#if MIN_VERSION_haskell_gi_base(0,20,1)
    Just obj <- builderGetObject builder name
#else
    Just obj <- nullToNothing $ builderGetObject builder name
#endif
    liftIO $ fromJust <$> castTo ctor obj
