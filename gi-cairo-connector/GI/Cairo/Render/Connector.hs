-----------------------------------------------------------------------------
-- |
-- Module      :  GI.Cairo.Render.Connector
-- Copyright   :  (c) Kilian Kilger, Iñaki García Etxebarria 2018 
-- License     :  LGPL-2.1
--
-- Maintainer  :  kkilger@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This library contains glue code to interconnect Haskell GI, i.e. the gi-cairo
-- library and gi-cairo-render, i.e. the hand written cairo bindings for Haskell.
----------------------------------------------------------------------------- 
module GI.Cairo.Render.Connector (
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

-- | This function is used to call a function in the 'GI.Cairo.Render.Render' monad
-- from the draw function of GTK+. It takes a 'GI.Cairo.Structs.Context' (as it
-- appears in [gi-cairo](https://hackage.haskell.org/package/gi-cairo)) and a 
-- 'GI.Cairo.Render.Render' action (as it appears in gi-cairo-render)
-- and renders the `Render` context inside the given context.
-- 
renderWithContext :: MonadIO m => 
                        Render a                   -- ^ Render action
                     -> GI.Cairo.Context -- ^ Context from gi-cairo 
                     -> m a                        -- ^ Result of the action
renderWithContext r ct = liftIO $ withManagedPtr ct $ \p ->
  runReaderT (runRender r) (Cairo (castPtr p))

makeContext :: Ptr Cairo -> IO GI.Cairo.Context
makeContext ptr = do ptr <- newManagedPtr (castPtr ptr) (return ()) 
                     return $ GI.Cairo.Context ptr

-- | This function is used to call back to functions of GI.Pango or GI.PangoCairo
-- or other Haskell GI libraries from inside the `Cairo.Render` action. 
-- Example:
-- 
-- > draw :: Cairo.Render () 
-- > draw = do ...
-- >           context <- getContext
-- >           somePangoFunction context
-- 
getContext :: Render GI.Cairo.Context -- ^ Underlying GI.Cairo.Context 
getContext = do cairo <- asks unCairo
                liftIO $ makeContext cairo

-- | This function is used to "lift" functions of GI.Pango or GI.PangoCairo
-- to the Render monad.
-- Example:
-- 
-- > draw :: Cairo.Render () 
-- > draw = do ...
-- >           toRender somePangoFunction
--  
toRender :: (GI.Cairo.Context -> IO a) -> Render a
toRender fun = do context <- getContext
                  liftIO $ fun context
