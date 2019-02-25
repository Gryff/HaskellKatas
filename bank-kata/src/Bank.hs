module Bank where

import Control.Monad.State
import Data.Time

data Transaction = Deposit Int UTCTime | Withdrawal Int UTCTime deriving (Eq, Show)
type TransactionRepo m = StateT [Transaction] m

deposit :: Monad m => Int -> TransactionRepo m ()
deposit amount = modify $ \transactions -> transactions ++ [Deposit amount firstOfJan2018]

withdraw :: Monad m => Int -> TransactionRepo m ()
withdraw amount = modify $ \transactions -> transactions ++ [Withdrawal amount firstOfJan2018]

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

firstOfJan2018 = UTCTime (fromGregorian 2018 01 01) (secondsToDiffTime 0)

