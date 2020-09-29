module SevenExercise (fnall, fnany, fntakewhile, fnmap) where

fnall :: (a -> Bool) -> [a] -> Bool
fnall fn [] = False
fnall fn [x] = fn x
fnall fn (x:xs) = if fn x
                  then fnall fn xs
                  else False

fnallimproved f [] = False
fnallimproved f (x:xs) = f x && fnallimproved f xs

fnany f [] = False
fnany f (x:xs) = f x && fnany f xs


fntakewhile f [] = []
fntakewhile f (x:xs) | f x = x : fntakewhile f xs
                     | True = fntakewhile f xs

fnmap :: (a -> b) -> [a] -> [b]
fnmap f lst = foldr (\x acc -> f x : acc) [] lst
