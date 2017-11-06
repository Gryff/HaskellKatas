module Rover
    ( move
    ) where

data Direction = North | East | South | West
type Position = (Int, Int, Direction)
type Obstacle = (Int, Int)

move :: String -> [Obstacle] -> String
move commands obstacles = toString (movement commands obstacles initialPosition)

movement :: String -> [Obstacle] -> Position -> Position
movement commands obstacles position = foldl (executeCommand obstacles) position commands

executeCommand :: [Obstacle] -> Position -> Char -> Position
executeCommand _ position command
  | command == 'M' = moveForward position
  | command == 'L' = rotateLeft position
  | command == 'R' = rotateRight position

moveForward :: Position -> Position
moveForward (x, y, North) = (x, (y + 1) `mod` 10, North)
moveForward (x, y, East) = ((x + 1) `mod` 10, y, East)
moveForward (x, y, West) = ((x - 1) `mod` 10, y, West)
moveForward (x, y, South) = (x, (y - 1) `mod` 10, South)

rotateLeft :: Position -> Position
rotateLeft (x, y, North) = (x, y, West)
rotateLeft (x, y, West) = (x, y, South)
rotateLeft (x, y, South) = (x, y, East)
rotateLeft (x, y, East) = (x, y, North)

rotateRight :: Position -> Position
rotateRight (x, y, North) = (x, y, East)
rotateRight (x, y, East) = (x, y, South)
rotateRight (x, y, South) = (x, y, West)
rotateRight (x, y, West) = (x, y, North)

initialPosition :: Position
initialPosition = (0, 0, North)

toString :: Position -> String
toString (x, y, dir) = show x ++ "," ++ show y ++ "," ++ dirToString dir

dirToString :: Direction -> String
dirToString North = "N"
dirToString East = "E"
dirToString South = "S"
dirToString West = "W"

