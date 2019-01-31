module Bank where

import Control.Monad.State

data Transaction = Deposit Int | Withdrawal Int deriving (Eq, Show)
type TransactionRepo m = StateT [Transaction] m

deposit :: Monad m => Int -> TransactionRepo m ()
deposit amount = modify $ \transactions -> transactions ++ [Deposit amount]

withdraw :: Monad m => Int -> TransactionRepo m ()
withdraw amount = modify $ \transactions -> transactions ++ [Withdrawal amount]

getStatement :: Monad m => TransactionRepo m String
getStatement = gets toStatement

toStatement :: [Transaction] -> String
toStatement transactions = unlines $ map stringifyTransaction (zip balance transactions)
  where balance = tail $ scanl calculateBalance 0 transactions

printStatement :: Monad m => (String -> m ()) -> TransactionRepo m ()
printStatement printer = do
  transactions <- get
  let statement = toStatement transactions
  lift $ printer statement

calculateBalance :: Int -> Transaction -> Int
calculateBalance currentBalance (Deposit amount) = currentBalance + amount
calculateBalance currentBalance (Withdrawal amount) = currentBalance - amount

stringifyTransaction :: (Int, Transaction) -> String
stringifyTransaction (currentBalance, (Deposit amount)) = "Desposited " ++ (show amount) ++ " | Balance " ++ (show currentBalance)
stringifyTransaction (currentBalance, (Withdrawal amount)) = "Withdrew " ++ (show amount) ++ " | Balance " ++ (show currentBalance)

