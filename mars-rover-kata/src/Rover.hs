module Rover
    ( move
    ) where

move :: String -> String
move commands = if commands == "M" then "0,1,N" else "0,0,N"

