module ReaderChap where

import Control.Applicative
import Data.Char
import Control.Monad.State.Lazy

boop = (* 2)
doop = (+ 10)

bip = boop . doop

trial :: Integer -> Integer
trial = do
  a <- boop
  b <- doop
  return (a + b)

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = do
  a <- cap
  b <- rev
  return (a, b)

x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

mlookup :: Eq a => a -> [(a, b)] -> Maybe b
mlookup _ [] = Nothing
mlookup x1 ((x2, y1):xs) = if x1 == x2 then
                               Just y1
                             else
                               mlookup x1 xs

stateTest = do
  (v, s) <- get :: State Int Int 
--  y <- 10 + x 
  put 130
  return x
