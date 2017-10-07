module Rover
    ( move
    ) where

type Direction = Char
type Position = (Int, Int, Direction)

move :: String -> String
move commands = toString (movement commands initialPosition)

movement :: String -> Position -> Position
movement commands position = foldr executeCommand position commands

executeCommand :: Char -> Position -> Position
executeCommand command (x, y, dir)
  | command == 'M' = (x, y + 1, dir)
  | command == 'L' = (x, y, rotateLeft dir)
  | command == 'R' = (x, y, rotateRight dir)

rotateLeft :: Direction -> Direction
rotateLeft dir
  | dir == 'N' = 'W'
  | dir == 'W' = 'S'
  | dir == 'E' = 'N'
  | dir == 'S' = 'E'

rotateRight :: Direction -> Direction
rotateRight dir
  | dir == 'N' = 'E'

initialPosition :: Position
initialPosition = (0, 0, 'N')

toString :: Position -> String
toString (x, y, dir) = show x ++ "," ++ show y ++ "," ++ [dir]

