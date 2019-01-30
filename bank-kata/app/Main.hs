module Main where

import Control.Monad.State

import Bank

newBank = []

main :: IO ()
main = do
  let statement = evalState doTheStuff newBank
  putStr statement

doTheStuff = do
  deposit 200
  withdraw 100
  deposit 3000
  getStatement
