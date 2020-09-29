module SingleLinkedList (pureFunction, total, hailStone, hailStoneSeq, sumEveryTwo, halve, luhnDouble) where

pureFunction :: Int -> Int -> Int
pureFunction a b = a + b

total :: Int -> Int
total 0 = 0
total n = n + total (n - 1)

hailStone :: Integer -> Integer
hailStone n
    | n `mod` 2 == 0 = n `div` 2
    | otherwise = 3 * n + 1

hailStoneSeq :: Integer -> [Integer]
hailStoneSeq 1 = [1]
hailStoneSeq n = n : hailStoneSeq (hailStone n)

intListLength :: [Integer] -> Integer
intListLength [] = 0
intListLength (_:xs) = 1 + intListLength xs

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo [] = []
sumEveryTwo [x] = []
sumEveryTwo (x:(y:zs)) = (x+y) : sumEveryTwo zs

hailStoneLen :: Integer -> Integer
hailStoneLen n = intListLength (hailStoneSeq n) - 1

data Thing = Shoe
    | Ship
    | SealingWax
    | Cabbage
    | King

halve :: [a] -> ([a], [a])
halve [x,y] = ([x], [y])
halve lst = (xs, ys)
  where len = div (length lst) 2
        xs = take len lst
        ys = drop len lst

luhnDouble x | 2 * x > 9 = 2 * x - 9
             | otherwise = 2 * x

appendList :: Eq a => [a] -> [a] -> [a]
appendList x y | x == [] = y
               | y == [] = x
               | otherwise = let xh = head x
                                 xt = tail x in
                               xh:(appendList xt y)

insert :: Ord a => a -> [a] -> [a]
insert x y | y == [] = [x]
           | otherwise = let yh = head y
                             yt = tail y in
                           if x < yh then
                             x : yh : yt
                           else
                             yh : (insert x yt)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                 smaller = [a | a <- xs, a <= x]
                 larger = [b | b <- xs, b > x]

merge :: Ord a => [a] -> [a] -> [a]
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys) = if x < y then
                        x : merge xs (y:ys)
                      else
                        y : merge (x:xs) ys
