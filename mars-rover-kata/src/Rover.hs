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
executeCommand _ position command
  | command == 'M' = moveForward position
  | command == 'L' = Right $ rotateLeft position
  | command == 'R' = Right $ rotateRight position

moveForward :: Position -> Either Position Position
moveForward (x, y, North) = Right (x, (y + 1) `mod` 10, North)
moveForward (x, y, East)  = Right ((x + 1) `mod` 10, y, East)
moveForward (x, y, West)  = Right ((x - 1) `mod` 10, y, West)
moveForward (x, y, South) = Right (x, (y - 1) `mod` 10, South)

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
toString (Right (x, y, dir)) = show x ++ "," ++ show y ++ "," ++ dirToString dir

dirToString :: Direction -> String
dirToString North = "N"
dirToString East = "E"
dirToString South = "S"
dirToString West = "W"

