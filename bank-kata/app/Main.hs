module Main where

import Control.Monad.State

import Bank

newBank = []

main :: IO ()
main = do
  runStateT doTheStuff newBank
  pure ()

doTheStuff = do
  deposit 200
  withdraw 100
  deposit 3000
  printStatement

