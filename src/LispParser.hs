module LispParser (sumOfNum) where

import Text.ParserCombinators.ReadP

operations :: [String]
operations = ["def"]

isOperation :: String -> Bool
isOperation token =
  elem token operations

sumOfNum :: String -> Maybe Int
sumOfNum s = case s of
  [] -> Nothing
  x -> Just $ length x

effective = do
  s <- readLn "SomeString"
  n <- sumOfNum s
  print n
