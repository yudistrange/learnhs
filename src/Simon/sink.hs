module Sink where

import Control.Concurrent

addFive :: Int -> Int
addFive = (+ 5)

myPutLine :: [Char] -> IO [Char]
myPutLine [] = return []
myPutLine ('\n':_) = return []
myPutLine (x:xs) = (putChar x) >> myPutLine xs

printInDifferentThread :: String -> IO ThreadId
printInDifferentThread s = do
  forkIO $ threadDelay 5000 >>= (\_ -> putStr s)
