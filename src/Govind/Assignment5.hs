module Assignment5 where

data User = User { userId :: String, name :: String, age :: Int, email :: String } deriving (Show)

type ValidatedUser = Either String User

validateId :: User -> ValidatedUser
validateId user = Left "Invalid Id"

validateName :: User -> ValidatedUser
validateName user
  | (nameLength < 3) = Left "Name too short"
  | (nameLength - spaceCount > 0) = Left "Name has too many spaces"
  | otherwise = Right user
  where
    nameLength = length $ name user
    spaceCount = length $ filter (\x -> x == ' ') $ name user

validateAge :: User -> ValidatedUser
validateAge user
  | 0 > age user = Left "Age should be between 0 and 150"
  | 150 < age user = Left "Age should be between 0 and 150"
  | otherwise = Right user

validateEmail :: User -> ValidatedUser
validateEmail user
  | elem '@' $ email user = Right user
  | otherwise = Left "Email should contain @"

-- validate :: User -> ValidatedUser
-- validate user = validateEmail <$> user <*> validateAge <*> validateName

-- Get a validator that chains failures
