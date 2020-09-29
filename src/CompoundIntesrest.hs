module CompoundInterest where

recurring :: Int -> Float -> Float -> Float
recurring time principal interest
  | time > 0 = (lumpsum time principal interest) + (recurring (time - 1) principal interest)
  | otherwise = 0

lumpsum :: Int -> Float -> Float -> Float
lumpsum time principal interest = principal * (1 + interest / 100) ^ time