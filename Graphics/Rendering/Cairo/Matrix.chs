-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Cairo.Matrix
-- Copyright   :  (c) Paolo Martini 2005
-- License     :  BSD-style (see cairo/COPYRIGHT)
--
-- Maintainer  :  p.martini@neuralnoise.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Matrix math
-----------------------------------------------------------------------------

module Graphics.Rendering.Cairo.Matrix (
    Matrix(Matrix)
  , MatrixPtr
  , identity
  , translate
  , scale
  , rotate
  , transformDistance
  , transformPoint
  , scalarMultiply
  , adjoint
  , invert
  ) where

import Foreign hiding (rotate)
import Foreign.C

-- | Representation of a 2-D affine transformation.
--
--  The Matrix type represents a 2x2 transformation matrix along with a
--  translation vector. @Matrix a1 a2 b1 b2 c1 c2@ describes the
--  transformation of a point with coordinates x,y that is defined by
--
--  >   / x' \  =  / a1 b1 \  / x \  + / c1 \
--  >   \ y' /     \ a2 b2 /  \ y /    \ c2 /
--
--  or
--
--  >   x' =  a1 * x + b1 * y + c1
--  >   y' =  a2 * x + b2 * y + c2

data Matrix = Matrix { xx :: !Double, yx :: !Double,
                       xy :: !Double, yy :: !Double,
                       x0 :: !Double, y0 :: !Double }
  deriving (Show, Eq)

{#pointer *cairo_matrix_t as MatrixPtr -> Matrix#}

instance Storable Matrix where
  sizeOf _ = {#sizeof cairo_matrix_t#}
  alignment _ = alignment (undefined :: CDouble)
  peek p = do
    xx <- {#get cairo_matrix_t->xx#} p
    yx <- {#get cairo_matrix_t->yx#} p
    xy <- {#get cairo_matrix_t->xy#} p
    yy <- {#get cairo_matrix_t->yy#} p
    x0 <- {#get cairo_matrix_t->x0#} p
    y0 <- {#get cairo_matrix_t->y0#} p
    return $ Matrix (realToFrac xx) (realToFrac yx)
                    (realToFrac xy) (realToFrac yy)
                    (realToFrac x0) (realToFrac y0)

  poke p (Matrix xx yx xy yy x0 y0) = do
    {#set cairo_matrix_t->xx#} p (realToFrac xx)
    {#set cairo_matrix_t->yx#} p (realToFrac yx)
    {#set cairo_matrix_t->xy#} p (realToFrac xy)
    {#set cairo_matrix_t->yy#} p (realToFrac yy)
    {#set cairo_matrix_t->x0#} p (realToFrac x0)
    {#set cairo_matrix_t->y0#} p (realToFrac y0)
    return ()

instance Num Matrix where
  (*) (Matrix xx yx xy yy x0 y0) (Matrix xx' yx' xy' yy' x0' y0') =
    Matrix (xx * xx' + yx * xy')
           (xx * yx' + yx * yy')
           (xy * xx' + yy * xy')
           (xy * yx' + yy * yy')
           (x0 * xx' + y0 * xy' + x0')
           (x0 * yx' + y0 * yy' + y0')

  (+) = pointwise2 (+)
  (-) = pointwise2 (-)

  negate = pointwise negate
  abs    = pointwise abs
  signum = pointwise signum

  -- this definition of fromInteger means that 2*m = scale 2 m
  -- and it means 1 = identity
  fromInteger n = Matrix (fromInteger n) 0 0 (fromInteger n) 0 0

{-# INLINE pointwise #-}
pointwise f (Matrix xx yx xy yy x0 y0) =
  Matrix (f xx) (f yx) (f xy) (f yy) (f x0) (f y0)

{-# INLINE pointwise2 #-}
pointwise2 f (Matrix xx yx xy yy x0 y0) (Matrix xx' yx' xy' yy' x0' y0') =
  Matrix (f xx xx') (f yx yx') (f xy xy') (f yy yy') (f x0 x0') (f y0 y0')

identity :: Matrix
identity = Matrix 1 0 0 1 0 0

translate :: Double -> Double -> Matrix -> Matrix
translate tx ty m = m * (Matrix 1 0 0 1 tx ty)

scale :: Double -> Double -> Matrix -> Matrix
scale sx sy m = m * (Matrix sx 0 0 sy 0 0)

rotate :: Double -> Matrix -> Matrix
rotate r m = m * (Matrix c s (-s) c 0 0)
  where s = sin r
        c = cos r

transformDistance :: Matrix -> (Double,Double) -> (Double,Double)
transformDistance (Matrix xx yx xy yy _ _) (dx,dy) =
  newX `seq` newY `seq` (newX,newY)
  where newX = xx * dx + xy * dy
        newY = yx * dx + yy * dy

transformPoint :: Matrix -> (Double,Double) -> (Double,Double)
transformPoint (Matrix xx yx xy yy x0 y0) (dx,dy) =
  newX `seq` newY `seq` (newX,newY)
  where newX = xx * dx + xy * dy + x0
        newY = yx * dx + yy * dy + y0

scalarMultiply :: Double -> Matrix -> Matrix
scalarMultiply scalar = pointwise (*scalar)

adjoint :: Matrix -> Matrix
adjoint (Matrix a b c d tx ty) =
  Matrix d (-b) (-c) a (c*ty - d*tx) (b*tx - a*ty)

invert :: Matrix -> Matrix
invert m@(Matrix xx yx xy yy _ _) = scalarMultiply (recip det) $ adjoint m
  where det = xx*yy - yx*xy
