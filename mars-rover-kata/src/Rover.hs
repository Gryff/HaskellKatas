module Rover where

import Control.Monad

data Direction = North | East | South | West
type Position = (Int, Int, Direction)
type Obstacle = (Int, Int)

move :: String -> [Obstacle] -> String
move commands obstacles = toString (movement commands obstacles initialPosition)

movement :: String -> [Obstacle] -> Position -> Either Position Position
movement commands obstacles position = foldM (executeCommand obstacles) position commands

executeCommand :: [Obstacle] -> Position -> Char -> Either Position Position
executeCommand obstacles position command
  | command == 'M' = moveForward obstacles position
  | command == 'L' = Right $ rotateLeft position
  | command == 'R' = Right $ rotateRight position

moveForward :: [Obstacle] -> Position -> Either Position Position
moveForward obstacles position
  | isCollision obstacles (nextPosition position) = Left position
  | otherwise = Right (nextPosition position)

nextPosition :: Position -> Position
nextPosition (x, y, North) = (x, (y + 1) `mod` 10, North)
nextPosition (x, y, West)  = ((x - 1) `mod` 10, y, West)
nextPosition (x, y, South) = (x, (y - 1) `mod` 10, South)
nextPosition (x, y, East) = ((x + 1) `mod` 10, y, East)

isCollision :: [Obstacle] -> Position -> Bool
isCollision obstacles (x, y, _) = elem (x, y) obstacles

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

toString :: Either Position Position -> String
toString (Right position) = positionToString position
toString (Left position) = "O," ++ positionToString position

positionToString :: Position -> String
positionToString (x, y, dir) = show x ++ "," ++ show y ++ "," ++ dirToString dir

dirToString :: Direction -> String
dirToString North = "N"
dirToString East = "E"
dirToString South = "S"
dirToString West = "W"

