module Bank where

import Control.Monad.State

data Transaction = Deposit Int | Withdrawal Int deriving (Eq, Show)

deposit :: Int -> State [Transaction] ()
deposit amount = modify $ \transactions -> transactions ++ [Deposit amount]

withdraw :: Int -> State [Transaction] ()
withdraw amount = modify $ \transactions -> transactions ++ [Withdrawal amount]

getStatement :: State [Transaction] String
getStatement = gets $ \transactions -> toStatement transactions

toStatement :: [Transaction] -> String
toStatement transactions = unlines $ map stringifyTransaction transactions

stringifyTransaction :: Transaction -> String
stringifyTransaction (Deposit amount) = "Desposited " ++ (show amount)
stringifyTransaction (Withdrawal amount) = "Withdrew " ++ (show amount)

