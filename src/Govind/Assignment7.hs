module Assignment7 where

data ChainedEither a b = ChainedLeft [a] | ChainedRight b deriving Show

instance Functor (ChainedEither a) where
  fmap _fn (ChainedLeft x) = ChainedLeft x
  fmap fn (ChainedRight y) = ChainedRight (fn y)

instance Applicative (ChainedEither a) where
  ChainedLeft x <*> ChainedRight _ = ChainedLeft x
  ChainedLeft x <*> ChainedLeft y = ChainedLeft (x ++ y)
  ChainedRight _ <*> ChainedLeft y = ChainedLeft y
  ChainedRight fn <*> ChainedRight y = ChainedRight (fn y)

  pure x = ChainedRight x

data User = User String Int deriving Show

validateNameX :: String -> (Int -> User)
validateNameX name
  | length name > 5 = User name
  | otherwise = User name
--  | otherwise = ChainedLeft ["Invalid username"]

--validateAgeX :: Int -> (String -> User)
--validateAgeX age = User age

validateName :: String -> ChainedEither String String
validateName name
  | length name > 5 = pure name
  | otherwise = ChainedLeft ["Invalid username"]

validateAge :: Int -> ChainedEither String Int
validateAge age
  | age > 150 = ChainedLeft ["Age is more than 150"]
  | age > 0 = pure age
  | otherwise = ChainedLeft ["Age is negative"]
