module Bank where

import Control.Monad.State
import Data.Time

data Transaction = Deposit Int UTCTime | Withdrawal Int UTCTime deriving (Eq, Show)
type TransactionRepo m = StateT [Transaction] m

deposit :: (Monad m, MonadCurrentDateTime m) => Int -> TransactionRepo m ()
deposit amount = do
  now <- lift currentDateTime
  modify $ \transactions -> transactions ++ [Deposit amount now]

withdraw :: (Monad m, MonadCurrentDateTime m) => Int -> TransactionRepo m ()
withdraw amount = do
  now <- lift currentDateTime
  modify $ \transactions -> transactions ++ [Withdrawal amount now]

getStatement :: Monad m => TransactionRepo m String
getStatement = gets toStatement

toStatement :: [Transaction] -> String
toStatement transactions = unlines $ map stringifyTransaction (zip balance transactions)
  where balance = tail $ scanl calculateBalance 0 transactions

printStatement :: (Monad m, MonadStatementPrinter m) => TransactionRepo m ()
printStatement = do
  transactions <- get
  let statement = toStatement transactions
  lift $ printSt statement

calculateBalance :: Int -> Transaction -> Int
calculateBalance currentBalance (Deposit amount _) = currentBalance + amount
calculateBalance currentBalance (Withdrawal amount _) = currentBalance - amount

stringifyTransaction :: (Int, Transaction) -> String
stringifyTransaction (currentBalance, (Deposit amount _)) = "Desposited " ++ (show amount) ++ " | Balance " ++ (show currentBalance)
stringifyTransaction (currentBalance, (Withdrawal amount _)) = "Withdrew " ++ (show amount) ++ " | Balance " ++ (show currentBalance)

class MonadStatementPrinter m where
  printSt :: String -> m ()

instance MonadStatementPrinter IO where
  printSt = putStr

class MonadCurrentDateTime m where
  currentDateTime :: m UTCTime

instance MonadCurrentDateTime IO where
  currentDateTime = getCurrentTime

