module Start (helloWorld) where

hello :: String
hello = "Hello World"

helloWorld :: String -> String
helloWorld name = hello <> name
