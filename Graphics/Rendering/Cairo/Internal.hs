{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.Internal
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see cairo/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Direct bindings to the cairo library.
-----------------------------------------------------------------------------
-- #hide
--
module Graphics.Rendering.Cairo.Internal (
    Render(..), bracketR
  , module Graphics.Rendering.Cairo.Types
  , module Graphics.Rendering.Cairo.Internal.Drawing.Cairo
  , module Graphics.Rendering.Cairo.Internal.Drawing.Paths
  , module Graphics.Rendering.Cairo.Internal.Drawing.Patterns
  , module Graphics.Rendering.Cairo.Internal.Drawing.Text
  , module Graphics.Rendering.Cairo.Internal.Drawing.Transformations
  , module Graphics.Rendering.Cairo.Internal.Fonts.FontOptions
  , module Graphics.Rendering.Cairo.Internal.Surfaces.Image
  , module Graphics.Rendering.Cairo.Internal.Surfaces.PDF
  , module Graphics.Rendering.Cairo.Internal.Surfaces.PNG
  , module Graphics.Rendering.Cairo.Internal.Surfaces.PS
  , module Graphics.Rendering.Cairo.Internal.Surfaces.SVG
  , module Graphics.Rendering.Cairo.Internal.Surfaces.Surface
  , module Graphics.Rendering.Cairo.Internal.Region
  , module Graphics.Rendering.Cairo.Internal.Utilities

  ) where

import Graphics.Rendering.Cairo.Types
import Graphics.Rendering.Cairo.Internal.Drawing.Cairo
import Graphics.Rendering.Cairo.Internal.Drawing.Paths
import Graphics.Rendering.Cairo.Internal.Drawing.Patterns
import Graphics.Rendering.Cairo.Internal.Drawing.Text
import Graphics.Rendering.Cairo.Internal.Drawing.Transformations
import Graphics.Rendering.Cairo.Internal.Fonts.FontOptions
import Graphics.Rendering.Cairo.Internal.Surfaces.Image
import Graphics.Rendering.Cairo.Internal.Surfaces.PDF
import Graphics.Rendering.Cairo.Internal.Surfaces.PNG
import Graphics.Rendering.Cairo.Internal.Surfaces.PS
import Graphics.Rendering.Cairo.Internal.Surfaces.SVG
import Graphics.Rendering.Cairo.Internal.Surfaces.Surface
import Graphics.Rendering.Cairo.Internal.Region
import Graphics.Rendering.Cairo.Internal.Utilities

import Control.Monad.Reader
import Control.Applicative
import Control.Exception (bracket)

-- | The Render monad. All drawing operations take place in a Render context.
-- You can obtain a Render context for a 'Surface' using 'renderWith'.
--
newtype Render m = Render { runRender :: ReaderT Cairo IO m }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Cairo)

{-# INLINE bracketR #-}
bracketR :: IO a -> (a -> IO b) -> (a -> Render c) -> Render c
bracketR begin end action =
  Render $
  ReaderT $ \r ->
  bracket begin end
          (\s -> runReaderT (runRender $ action s) r)

