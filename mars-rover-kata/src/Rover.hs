module Rover
    ( move
    ) where

type Position = (Int, Int, Char)

move :: String -> String
move commands = toString (movement commands initialPosition)

movement :: String -> Position -> Position
movement commands position = foldr executeCommand position commands

executeCommand :: Char -> Position -> Position
--executeCommand command (x, y, dir) = if command == 'M' then (x, y + 1, dir) else (x, y, rotateLeft dir)
executeCommand command (x, y, dir)
  | command == 'M' = (x, y + 1, dir)
  | otherwise = (x, y, rotateLeft dir)

rotateLeft :: Char -> Char
rotateLeft dir = if dir == 'N' then 'W' else 'S'

initialPosition :: Position
initialPosition = (0, 0, 'N')

toString :: Position -> String
toString (x, y, dir) = show x ++ "," ++ show y ++ "," ++ [dir]

