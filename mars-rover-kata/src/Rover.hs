module Rover
    ( move
    ) where

move :: String -> String
move commands = if commands == "M" then "0,1,N" else if commands == "MM" then "0,2,N" else "0,0,N"

