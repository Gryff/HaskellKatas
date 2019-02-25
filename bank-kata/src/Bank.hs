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

printStatement :: (Monad m, MonadStatementPrinter m) => TransactionRepo m ()
printStatement = do
  transactions <- get
  let statement = toStatement transactions
  lift $ printSt statement

toStatement :: [Transaction] -> String
toStatement transactions = unlines $ [statementHeader] ++ statementBody
  where balance = tail $ scanl calculateBalance 0 transactions
        statementBody = map stringifyTransaction (zip balance transactions)

statementHeader = "date       || credit || debit || balance"

calculateBalance :: Int -> Transaction -> Int
calculateBalance currentBalance (Deposit amount _) = currentBalance + amount
calculateBalance currentBalance (Withdrawal amount _) = currentBalance - amount

stringifyTransaction :: (Int, Transaction) -> String
stringifyTransaction (currentBalance, (Deposit amount date)) =
  (formatTime defaultTimeLocale "%d/%m/%Y" date) ++ " || " ++
  (show amount) ++ ".00" ++ " || || " ++
  (show currentBalance) ++ ".00"

stringifyTransaction (currentBalance, (Withdrawal amount date)) =
  (formatTime defaultTimeLocale "%d/%m/%Y" date) ++ " || || " ++
  (show amount) ++ ".00" ++ " || " ++
  (show currentBalance) ++ ".00"

class MonadStatementPrinter m where
  printSt :: String -> m ()

instance MonadStatementPrinter IO where
  printSt = putStr

class MonadCurrentDateTime m where
  currentDateTime :: m UTCTime

instance MonadCurrentDateTime IO where
  currentDateTime = getCurrentTime

