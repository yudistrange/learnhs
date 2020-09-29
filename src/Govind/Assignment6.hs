module Assignment6 where

data User = User { id :: String, name :: String, age :: Int, email :: String }

type ValidatedUser = Either String User

validateId :: User -> Either String User
validateId user = Left "Invalid Id"

validateName :: User -> ValidatedUser
validateName user
  | (nameLength < 3) = Left "Name too short"
  | (nameLength < spaceCount) = Left "Name has too many spaces"
  | otherwise = Right user
  where
    nameLength = length $ name user
    spaceCount = 4
    
