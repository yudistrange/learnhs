module MarsRover () where

type Position = (Int, Int)
type Grid = [Position]
data Direction = North | South | East | West

isValidPosition :: Position -> Grid -> Bool
isValidPosition roverPosition grid = elem roverPosition grid

newPosition :: Position -> Direction -> Position
newPosition (x, y) North = (x, y + 1)
newPosition (x, y) South = (x, y - 1)
newPosition (x, y) East = (x + 1, y)
newPosition (x, y) West = (x - 1, y)

move :: Position -> Grid -> Direction -> Position
move roverPosition grid direction =
  let newPos = newPosition roverPosition direction in
    if isValidPosition newPos grid then
      newPos
    else
      roverPositionn
