module Bank where

import Control.Monad.State

data (Num a) => Bank a = Bank a deriving (Eq, Show)
data Transaction a = Deposit a | Withdrawal a deriving (Eq, Show)

newBank = []

deposit :: Int -> State [Transaction Int] ()
deposit amount = state $ \transactions -> ((), (Deposit amount) : transactions)

withdraw :: Int -> State [Transaction Int] ()
withdraw amount = state $ \transactions -> ((), (Withdrawal amount) : transactions)

