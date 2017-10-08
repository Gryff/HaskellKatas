module Rover
    ( move
    ) where

data Direction = North | East | South | West
type Position = (Int, Int, Direction)

move :: String -> String
move commands = toString (movement commands initialPosition)

movement :: String -> Position -> Position
movement commands position = foldl executeCommand position commands

executeCommand :: Position -> Char -> Position
executeCommand (x, y, dir) command
  | command == 'M' = moveForward (x, y, dir)
  | command == 'L' = (x, y, rotateLeft dir)
  | command == 'R' = (x, y, rotateRight dir)

moveForward :: Position -> Position
moveForward (x, y, North) = (x, (y + 1) `mod` 10, North)
moveForward (x, y, East) = ((x + 1) `mod` 10, y, East)
moveForward (x, y, West) = ((x - 1) `mod` 10, y, West)

rotateLeft :: Direction -> Direction
rotateLeft North = West
rotateLeft West = South
rotateLeft South = East
rotateLeft East = North

rotateRight :: Direction -> Direction
rotateRight North = East
rotateRight East = South
rotateRight South = West
rotateRight West = North

initialPosition :: Position
initialPosition = (0, 0, North)

toString :: Position -> String
toString (x, y, dir) = show x ++ "," ++ show y ++ "," ++ dirToString dir

dirToString :: Direction -> String
dirToString North = "N"
dirToString East = "E"
dirToString South = "S"
dirToString West = "W"

