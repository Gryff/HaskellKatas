module Rover
    ( move
    ) where

move :: String -> String
move commands = toString (movement commands initialPosition)

movement :: String -> (Int, Int, Char) -> (Int, Int, Char)
movement [] position = position
movement (command:commands) (x, y, dir) = movement commands (x, y + 1, dir)

initialPosition :: (Int, Int, Char)
initialPosition = (0, 0, 'N')

toString :: (Int, Int, Char) -> String
toString (x, y, dir) = show x ++ "," ++ show y ++ "," ++ [dir]

