module MyJson where

data MyJson = JStr String | JAsc [(String, MyJson)] | JArr [MyJson] | JNum Int deriving Show
