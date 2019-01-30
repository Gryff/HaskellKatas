module Bank where

import Control.Monad.State

data Transaction = Deposit Int | Withdrawal Int deriving (Eq, Show)
type TransactionRepo = State [Transaction]

deposit :: Int -> TransactionRepo ()
deposit amount = modify $ \transactions -> transactions ++ [Deposit amount]

withdraw :: Int -> TransactionRepo ()
withdraw amount = modify $ \transactions -> transactions ++ [Withdrawal amount]

getStatement :: TransactionRepo String
getStatement = gets toStatement

toStatement :: [Transaction] -> String
toStatement transactions = unlines $ map stringifyTransaction transactions

stringifyTransaction :: Transaction -> String
stringifyTransaction (Deposit amount) = "Desposited " ++ (show amount)
stringifyTransaction (Withdrawal amount) = "Withdrew " ++ (show amount)

