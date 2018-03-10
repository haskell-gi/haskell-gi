{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies  #-}
module Rectangle(
  Rectangle(..), 
  Point,
  IsARectangle(..),
  rIntersect,
  rIsInside,
  rFromBoundingBox,
  rToBoundingBox,
  rToTuple
) 
where

import qualified Graphics.UI.Gtk as GTK (Rectangle(..)) 

type Point a = (a,a)

data Num a => Rectangle a = Rectangle a a a a deriving(Show)

class Num a => IsARectangle r a | r -> a where
  rTopLeftX :: r -> a
  rTopLeftY :: r -> a
  rWidth :: r -> a
  rHeight :: r -> a 
  rBottomRightX :: r -> a
  rBottomRightX r = rTopLeftX r + rWidth r 
  rBottomRightY :: r -> a 
  rBottomRightY r = rTopLeftY r + rHeight r  
  rTopLeft :: r -> Point a
  rTopLeft rectangle = (rTopLeftX rectangle, rTopLeftY rectangle)
  rBottomRight :: r -> Point a
  rBottomRight rectangle = (rBottomRightX rectangle, rBottomRightY rectangle)
  
instance Num a => IsARectangle (Rectangle a) a where 
  rTopLeftX (Rectangle x _ _ _) = x
  rTopLeftY (Rectangle _ y _ _) = y 
  rWidth    (Rectangle _ _ width _) = width   
  rHeight   (Rectangle _ _ _ height) = height    

instance IsARectangle GTK.Rectangle Int where
  rTopLeftX (GTK.Rectangle x _ _ _) = x
  rTopLeftY (GTK.Rectangle _ y _ _) = y 
  rWidth    (GTK.Rectangle _ _ width _) = width   
  rHeight   (GTK.Rectangle _ _ _ height) = height     

instance IsARectangle (Double, Double, Double, Double) Int where
  rTopLeftX (x,_,_,_) = round x
  rTopLeftY (_,y,_,_) = round y 
  rWidth    (_,_,width,_) = round width   
  rHeight   (_,_,_,height) = round height     

rNotAbove :: (Ord a, Num a, IsARectangle r1 a, IsARectangle r2 a) => r1 -> r2 -> Bool
rNotAbove r1 r2 =  (&&) (rBottomRightX r1 >= rTopLeftX r2) (rBottomRightY r1 >= rTopLeftY r2)   

rDoIntersect :: (Ord a, Num a, IsARectangle r1 a, IsARectangle r2 a) => r1 -> r2 -> Bool
rDoIntersect r1 r2 = (&&) (rNotAbove r1 r2) (rNotAbove r2 r1) 

rComputeIntersection :: (Ord a, Num a, IsARectangle r1 a, IsARectangle r2 a) => r1 -> r2 -> Rectangle a
rComputeIntersection r1 r2 =  let topLeftX = max (rTopLeftX r1) (rTopLeftX r2)  
                                  topLeftY = max (rTopLeftY r1) (rTopLeftY r2)     
                                  btmRightX = min (rBottomRightX r1) (rBottomRightX r2)
                                  btmRightY = min (rBottomRightY r1) (rBottomRightY r2) 
                              in Rectangle topLeftX topLeftY (btmRightX - topLeftX) (btmRightY - topLeftY)     

rIntersect :: (Ord a, Num a, IsARectangle r1 a, IsARectangle r2 a) => r1 -> r2 -> Maybe (Rectangle a)
rIntersect r1 r2 = if rDoIntersect r1 r2 then Just $ rComputeIntersection r1 r2 
                   else Nothing

rIsInside :: (Ord a, Num a, IsARectangle r a)  => r -> Point a -> Bool
rIsInside rectangle (x,y) = rDoIntersect rectangle ( Rectangle x y 0 0 )

rFromBoundingBox :: (Num a, Num b) => (a -> b) -> (a, a, a, a) -> Rectangle b
rFromBoundingBox f (topLeftX, topLeftY, bottomRightX, bottomRightY) = 
  Rectangle (f topLeftX) (f topLeftY) (f (bottomRightX - topLeftX)) (f (bottomRightY - topLeftY))

rToBoundingBox :: (Num a, Num b) => (a -> b) -> Rectangle a -> (b, b, b, b)
rToBoundingBox f (Rectangle x y width height) = (f x, f y, f $ x + width, f $ y + height)

rToTuple :: (Num a, Num b) => (a->b) -> Rectangle a -> (b, b, b, b)
rToTuple f r =
  (f $ rTopLeftX r, f $ rTopLeftY r, f $ rWidth r, f $ rHeight r)
