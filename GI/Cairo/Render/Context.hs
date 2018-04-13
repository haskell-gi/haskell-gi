-----------------------------------------------------------------------------
-- |
-- Module      :  GI.Cairo.Render.Context
-- Copyright   :  (c) Kilian Kilger 2018
-- License     :  BSD-style (see cairo/COPYRIGHT)
--
-- Maintainer  :  Kilian Kilger (kkilger@gmail.com) 
-- Stability   :  experimental
-- Portability :  portable
--
-- Connection to the gi-cairo package
-----------------------------------------------------------------------------
module GI.Cairo.Render.Context (
   renderWithContext, getContext, toRender
) where

import qualified GI.Cairo(Context(..)) 
import Data.GI.Base(withManagedPtr, newManagedPtr)
import GI.Cairo.Render(Render)
import GI.Cairo.Render.Internal(Cairo(..), unCairo, runRender)
import Foreign.Ptr (castPtr)
import Control.Monad.Reader(MonadIO, runReaderT, asks)
import Control.Monad.IO.Class(liftIO)
import Foreign.Ptr(Ptr)

renderWithContext :: MonadIO m => GI.Cairo.Context -> Render a -> m a
renderWithContext ct r = liftIO $ withManagedPtr ct $ \p ->
  runReaderT (runRender r) (Cairo (castPtr p))

makeContext :: Ptr Cairo -> IO GI.Cairo.Context
makeContext ptr = do ptr <- newManagedPtr (castPtr ptr) (return ()) 
                     return $ GI.Cairo.Context ptr

getContext :: Render GI.Cairo.Context
getContext = do cairo <- asks unCairo
                liftIO $ makeContext cairo

toRender :: (GI.Cairo.Context -> IO a) -> Render a
toRender fun = do context <- getContext
                  liftIO $ fun context
