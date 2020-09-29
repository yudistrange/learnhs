module WikiHaskell where

import Text.Read

readingFromUser :: IO ()
readingFromUser = do
  input <- getLine
  let x = readMaybe input :: Maybe Integer
  case x of
    Just n -> putStrLn $ "Got something" <> show n
    Nothing -> putStrLn "Got nothing"

interactiveAddition = do
  putStrLn "Input numbers"
  n1 <- getLine
  n2 <- getLine
  case (+) <$> readMaybe n1 <*> readMaybe n2 of
    Just n -> putStrLn $ "Got something" <> show n
    Nothing -> putStrLn "Got nothing"
