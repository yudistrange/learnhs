module Functions where

add :: Int -> Int -> Int
add 5 _b = 5
add _a 10 = 0
add a b = a + b

-- List comprehensions
-- weirdNaturalNumbers
[x | x <- [1..200], x < 100, mod x 5 == 0, mod x 3 == 0, mod x 15 == 0]
