{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GI.Cairo.Render.Internal
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
module GI.Cairo.Render.Internal (
    Render(..), bracketR
  , module GI.Cairo.Render.Types
  , module GI.Cairo.Render.Internal.Drawing.Cairo
  , module GI.Cairo.Render.Internal.Drawing.Paths
  , module GI.Cairo.Render.Internal.Drawing.Patterns
  , module GI.Cairo.Render.Internal.Drawing.Text
  , module GI.Cairo.Render.Internal.Drawing.Transformations
  , module GI.Cairo.Render.Internal.Fonts.FontOptions
  , module GI.Cairo.Render.Internal.Surfaces.Image
  , module GI.Cairo.Render.Internal.Surfaces.PDF
  , module GI.Cairo.Render.Internal.Surfaces.PNG
  , module GI.Cairo.Render.Internal.Surfaces.PS
  , module GI.Cairo.Render.Internal.Surfaces.SVG
  , module GI.Cairo.Render.Internal.Surfaces.Surface
  , module GI.Cairo.Render.Internal.Region
  , module GI.Cairo.Render.Internal.Utilities

  ) where

import GI.Cairo.Render.Types
import GI.Cairo.Render.Internal.Drawing.Cairo
import GI.Cairo.Render.Internal.Drawing.Paths
import GI.Cairo.Render.Internal.Drawing.Patterns
import GI.Cairo.Render.Internal.Drawing.Text
import GI.Cairo.Render.Internal.Drawing.Transformations
import GI.Cairo.Render.Internal.Fonts.FontOptions
import GI.Cairo.Render.Internal.Surfaces.Image
import GI.Cairo.Render.Internal.Surfaces.PDF
import GI.Cairo.Render.Internal.Surfaces.PNG
import GI.Cairo.Render.Internal.Surfaces.PS
import GI.Cairo.Render.Internal.Surfaces.SVG
import GI.Cairo.Render.Internal.Surfaces.Surface
import GI.Cairo.Render.Internal.Region
import GI.Cairo.Render.Internal.Utilities

import Control.Monad.Reader
import Control.Applicative
import Control.Exception (bracket)

-- | The Render monad. All drawing operations take place in a Render context.
-- You can obtain a Render context for a 'Surface' using 'renderWith'.
--
newtype Render m = Render { runRender :: ReaderT Context IO m }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Context)

{-# INLINE bracketR #-}
bracketR :: IO a -> (a -> IO b) -> (a -> Render c) -> Render c
bracketR begin end action =
  Render $
  ReaderT $ \r ->
  bracket begin end
          (\s -> runReaderT (runRender $ action s) r)

