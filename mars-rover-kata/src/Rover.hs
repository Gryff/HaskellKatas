module Rover
    ( move
    ) where

type Position = (Int, Int, Char)

move :: String -> String
move commands = toString (movement commands initialPosition)

movement :: String -> Position -> Position
movement [] position = position
movement (command:commands) position = movement commands (executeCommand command position)

executeCommand :: Char -> Position -> Position
executeCommand command (x, y, dir) = if command == 'M' then (x, y + 1, dir) else (x, y, rotateLeft dir)

rotateLeft :: Char -> Char
rotateLeft dir = if dir == 'N' then 'W' else 'S'

initialPosition :: Position
initialPosition = (0, 0, 'N')

toString :: Position -> String
toString (x, y, dir) = show x ++ "," ++ show y ++ "," ++ [dir]

