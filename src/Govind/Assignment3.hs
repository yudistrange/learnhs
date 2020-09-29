module Assignment3 where

-- A function to map over a list
myListMap :: [a] -> (a -> b) -> [b]
myListMap [] _ = []
myListMap (x:xs) f = (f x) : (myListMap xs f)

-- A function to reduce list from left
myReduceLeftList :: [a] -> b -> (a -> b -> b) -> b
myReduceLeftList [] acc _ =  acc
myReduceLeftList (x:xs) acc f = myReduceLeftList xs newAcc f
  where newAcc = f x acc

-- A function to reduce list from right
myReduceRightList :: [a] -> b -> (a -> b -> b) -> b
myReduceRightList [] acc _ = acc
myReduceRightList (x:xs) acc f = f x (myReduceRightList xs acc f)

-- Polygon TypeClass with center and area functions
class Polygon a where
  center :: Num x => a -> (x, x)
  area :: Num x => a -> x

data Circle a  = Circle {cx :: a, cy :: a, radius :: a} deriving Show

instance Polygon Circle where
  
  center (Circle {cx = x, cy = y}) = (x, y)
  area circle = pi * r * r
    where r = radius circle

data Rectangle = Rectangle {rx1 :: Float, ry1 :: Float, rx2 :: Float, ry2 :: Float} deriving Show

instance Polygon Rectangle where
  center rect = ((rx1 rect + rx2 rect)/2 , (ry1 rect + ry2 rect)/2)
  area rect = abs ((rx1 rect - rx2 rect) * (ry1 rect - ry2 rect))

data Square = Square {sx1 :: Float, sy1 :: Float, sx2 :: Float, sy2 :: Float} deriving Show

instance Polygon Square where
  center square = ((sx1 square + sx2 square)/2 , (sy1 square + sy2 square)/2)
  area square = abs ((sx1 square - sx2 square) * (sy1 square - sy2 square))

newtype SquareRect = SquareRect Rectangle
