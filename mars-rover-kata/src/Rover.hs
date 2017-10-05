module Rover
    ( move
    ) where

type Position = (Int, Int, Char)

move :: String -> String
move commands = toString (movement commands initialPosition)

movement :: String -> Position -> Position
movement [] position = position
movement (command:commands) (x, y, dir) = movement commands (x, y + 1, dir)

initialPosition :: Position
initialPosition = (0, 0, 'N')

toString :: Position -> String
toString (x, y, dir) = show x ++ "," ++ show y ++ "," ++ [dir]

