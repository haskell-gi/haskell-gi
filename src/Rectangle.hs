module Rectangle(
  Rectangle(..), 
  rBottomRightX, 
  rBottomRightY, 
  rIntersect
) 
where

data Num a => Rectangle a = Rectangle {
  rTopLeftX :: a,
  rTopLeftY :: a,
  rWidth :: a,
  rHeight :: a
}

rBottomRightX :: Num a => Rectangle a -> a
rBottomRightX r = rTopLeftX r + rWidth r

rBottomRightY :: Num a => Rectangle a -> a
rBottomRightY r = rTopLeftY r + rHeight r 

rIntersect :: (Ord a, Num a) => Rectangle a -> Rectangle a -> Maybe (Rectangle a)
rIntersect r1 r2 = let notAbove r1 r2 =  rBottomRightX r1 >= rTopLeftX r2 && 
                                         rBottomRightY r1 >= rTopLeftY r2  
                       insideBounds r1 r2 = (&&) (notAbove r1 r2) (notAbove r2 r1)
                       doIntersect r1 r2 = let topLeftX = max (rTopLeftX r1) (rTopLeftX r2)  
                                               topLeftY = max (rTopLeftY r1) (rTopLeftY r2)     
                                               btmRightX = min (rBottomRightX r1) (rBottomRightX r2)
                                               btmRightY = min (rBottomRightY r1) (rBottomRightY r2) 
                                           in Rectangle { rTopLeftX = topLeftX,
                                                          rTopLeftY = topLeftY,
                                                          rWidth = btmRightX - topLeftX,
                                                          rHeight = btmRightY - topLeftY }   
                   in case insideBounds r1 r2 of 
                        True -> Just $ doIntersect r1 r2 
                        False -> Nothing
