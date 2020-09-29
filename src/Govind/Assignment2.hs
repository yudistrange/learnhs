module Assignment2 where

-- Read up on the newtype
-- Write a function for the MyJson data type

data MyJson =
  JStr String
  | JAsc [(String, MyJson)]
  | JArr [MyJson]
  | JNum Int
  deriving Show

wrapWithQuotes :: String -> String
wrapWithQuotes a = "'" <> a <> "'"

printMyJson :: MyJson -> String
printMyJson (JStr x) = wrapWithQuotes x
printMyJson (JNum x) = show x
printMyJson (JArr x) = "[ " <> (concat $ map (\e -> printMyJson e <> ", ") x) <> " ]"
printMyJson (JAsc x) = "{ " <> (concat $ map (\(k,v) -> (wrapWithQuotes k) <> ": " <> printMyJson v <> ", ") x) <> " }"
