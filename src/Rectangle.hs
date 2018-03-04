module Rectangle(Rectangle) where

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
rIntersect r1 r2 = let insideBounds r1 r2 = rBottomRightX r1 >= rTopLeftX r2 && 
                                            rBottomRightX r2 >= rTopLeftX r1 && 
                                            rBottomRightY r1 >= rTopLeftY r2 && 
                                            rBottomRightY r2 >= rTopLeftY r1   
                   in case insideBounds r1 r2 of 
                      True -> let topLeftX = max (rTopLeftX r1) (rTopLeftX r2)  
                                  topLeftY = max (rTopLeftY r1) (rTopLeftY r2)     
                                  btmRightX = min (rBottomRightX r1) (rBottomRightX r2)
                                  btmRightY = min (rBottomRightY r1) (rBottomRightY r2) 
                              in Just Rectangle { rTopLeftX = topLeftX,
                                                  rTopLeftY = topLeftY,
                                                  rWidth = btmRightX - topLeftX,
                                                  rHeight = btmRightY - topLeftY }
                      False -> Nothing
