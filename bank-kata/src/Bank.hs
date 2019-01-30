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
toStatement transactions = unlines $ map stringifyTransaction (zip balance transactions)
  where balance = tail $ scanl calculateBalance 0 transactions

calculateBalance :: Int -> Transaction -> Int
calculateBalance currentBalance (Deposit amount) = currentBalance + amount
calculateBalance currentBalance (Withdrawal amount) = currentBalance - amount

stringifyTransaction :: (Int, Transaction) -> String
stringifyTransaction (currentBalance, (Deposit amount)) = "Desposited " ++ (show amount) ++ " | Balance " ++ (show currentBalance)
stringifyTransaction (currentBalance, (Withdrawal amount)) = "Withdrew " ++ (show amount) ++ " | Balance " ++ (show currentBalance)

