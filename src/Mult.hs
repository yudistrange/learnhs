module Mult where

divide :: Num a => Eq a => a -> a -> Either a String
divide x y = if y == 0 then
               Right "Can't divide by zero"
             else
               Left (x / y)

data MyList a = Empty | List a

data MyBook = AnonBook | NamedBook String | NamedBookWithAuthor String String
