module Hangman (play) where

import System.IO

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

secretInput = do
  x <- getCh
  if x == '\n' then
    do putChar x
       return []
  else
    do putChar '-'
       xs <- secretInput
       return (x:xs)

match xs ys = [if elem x ys then x else '-' | x <- xs]

eval secret = do
  guessedWord <- getLine
  if guessedWord == secret then
    putStrLn "Yay! Guessed correctly"
  else
    do
      putStrLn $ match secret guessedWord
      eval secret

play = do
  putStrLn "Enter the secret word:"
  secret <- secretInput
  eval secret
