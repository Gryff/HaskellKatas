module Bank where

import Control.Monad.State

data Transaction = Deposit Int | Withdrawal Int deriving (Eq, Show)

deposit :: Int -> State [Transaction] ()
deposit amount = state $ \transactions -> ((), transactions ++ [Deposit amount])

withdraw :: Int -> State [Transaction] ()
withdraw amount = state $ \transactions -> ((), transactions ++ [Withdrawal amount])

getStatement :: State [Transaction] String
getStatement = state $ \transactions -> (toStatement transactions, transactions)

toStatement :: [Transaction] -> String
toStatement transactions = unlines $ map stringifyTransaction transactions

stringifyTransaction :: Transaction -> String
stringifyTransaction (Deposit amount) = "Desposited " ++ (show amount)
stringifyTransaction (Withdrawal amount) = "Withdrew " ++ (show amount)

