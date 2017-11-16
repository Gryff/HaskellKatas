module Bank where

data (Num a) => Bank a = Bank a deriving (Eq, Show)
data (Num a) => Deposit a = Deposit a
data (Num a) => Withdrawal a = Withdrawal a

deposit :: Deposit Int -> Bank Int -> Bank Int
deposit (Deposit amount) (Bank balance) = Bank (balance + amount)

withdraw :: Withdrawal Int -> Bank Int -> Bank Int
withdraw (Withdrawal amount) (Bank balance) = Bank (balance - amount)

