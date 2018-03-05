module Rectangle(
  Rectangle(..), 
  Point(..),
  IsARectangle(..),
  rIntersect,
  rIsInside
) 
where

import qualified Graphics.UI.Gtk as GTK (Rectangle(..)) 

data Num a => Point a = Point (a,a)

data Num a => Rectangle a = Rectangle a a a a; 

class Num a => IsARectangle r a | r -> a where
  rTopLeftX :: r -> a
  rTopLeftY :: r -> a
  rWidth :: r -> a
  rHeight :: r -> a 
  rTo :: r -> Rectangle a 
  rBottomRightX :: r -> a
  rBottomRightX r = rTopLeftX r + rWidth r 
  rBottomRightY :: r -> a 
  rBottomRightY r = rTopLeftY r + rHeight r  
  
instance Num a => IsARectangle (Rectangle a) a where 
  rTopLeftX (Rectangle x _ _ _) = x
  rTopLeftY (Rectangle _ y _ _) = y 
  rWidth    (Rectangle _ _ width _) = width   
  rHeight   (Rectangle _ _ _ height) = height    
  rTo       r = r

instance IsARectangle GTK.Rectangle Int where
  rTopLeftX (GTK.Rectangle x _ _ _) = x
  rTopLeftY (GTK.Rectangle _ y _ _) = y 
  rWidth    (GTK.Rectangle _ _ width _) = width   
  rHeight   (GTK.Rectangle _ _ _ height) = height     
  rTo       (GTK.Rectangle x y width height) = Rectangle x y width height

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
rIntersect r1 r2 = case rDoIntersect r1 r2 of 
                     True -> Just $ rComputeIntersection r1 r2 
                     False -> Nothing

rIsInside :: (Ord a, Num a, IsARectangle r a)  => r -> Point a -> Bool
rIsInside rectangle (Point (x,y)) = rDoIntersect rectangle ( Rectangle x y 0 0 )
