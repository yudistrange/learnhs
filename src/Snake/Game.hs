module Snake.Game where

import Control.Monad.Trans.State as ST

type Point = (Int, Int)

data Orientation = North | South | East | West deriving Show

data Snake = Snake
  { snakeHead :: Point,
    snakeLength :: Int,
    snakeOrientation :: Orientation
  } deriving Show

data Turn = TurnLeft | TurnRight deriving Show

data Grid = Grid {gridHeight :: Int, gridWidth:: Int} deriving Show

data GameState = GameState
  { snakes :: [Snake],
    grid :: Grid } deriving Show

addSnake :: ST.State GameState Snake
addSnake = do
  game <- ST.get
  let snake = Snake
        { snakeHead = (0,0),
          snakeLength = 2,
          snakeOrientation = North }
  ST.put $ game { snakes = snake : (snakes game)}
  return snake

moveSnake :: Snake -> Snake
moveSnake snake = snake { snakeHead = (0,1)}

step :: ST.State GameState GameState
step = do
  game <- ST.get
  ST.put $ game {snakes = updatedSnakes (snakes game)}
  return game
  where updatedSnakes snakeList = moveSnake <$> snakeList

addHundredAndReturnHalf :: ST.State Int Int
addHundredAndReturnHalf = do
  oldSum <- get
  put (oldSum + 100)
  return (div oldSum 2)

multThousandAndReturnDouble :: ST.State Int Int
multThousandAndReturnDouble = do
  oldSum <- get
  put (oldSum * 1000)
  return (oldSum * 2)
