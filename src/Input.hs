module Input (whatIsMyName) where

whatIsMyName :: IO ()
whatIsMyName = do
  name <- getLine
  putStr "You are goddamn right"
