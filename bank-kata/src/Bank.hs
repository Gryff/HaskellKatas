module Bank where

import Control.Monad.State

data Transaction a = Deposit a | Withdrawal a deriving (Eq, Show)

newBank = []

deposit :: Int -> State [Transaction Int] ()
deposit amount = state $ \transactions -> ((), transactions ++ [Deposit amount])

withdraw :: Int -> State [Transaction Int] ()
withdraw amount = state $ \transactions -> ((), transactions ++ [Withdrawal amount])

getStatement :: State [Transaction Int] String
getStatement = state $ \transactions -> (toStatement transactions, transactions)

toStatement :: [Transaction Int] -> String
toStatement transactions = unlines $ map stringifyTransaction transactions

stringifyTransaction :: Transaction Int -> String
stringifyTransaction (Deposit amount) = "Desposited " ++ (show amount)
stringifyTransaction (Withdrawal amount) = "Withdrew " ++ (show amount)

