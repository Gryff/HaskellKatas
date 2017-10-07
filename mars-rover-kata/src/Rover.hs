module Rover
    ( move
    ) where

type Position = (Int, Int, Char)

move :: String -> String
move commands = toString (movement commands initialPosition)

movement :: String -> Position -> Position
movement commands position = foldr executeCommand position commands

executeCommand :: Char -> Position -> Position
executeCommand command (x, y, dir)
  | command == 'M' = (x, y + 1, dir)
  | otherwise = (x, y, rotateLeft dir)

rotateLeft :: Char -> Char
rotateLeft dir
  | dir == 'N' = 'W'
  | dir == 'W' = 'S'
  | dir == 'E' = 'N'
  | dir == 'S' = 'E'

initialPosition :: Position
initialPosition = (0, 0, 'N')

toString :: Position -> String
toString (x, y, dir) = show x ++ "," ++ show y ++ "," ++ [dir]

