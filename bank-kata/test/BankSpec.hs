{-# LANGUAGE FlexibleInstances #-}

module BankSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor.Identity
import Data.Time

import Bank

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

--doStatement :: TransactionRepo (Writer String) ()
--doStatement = do
  --deposit 200
  --withdraw 100
  --deposit 3000
  --printStatement

newBank = []

spec :: Spec
spec = do
  describe "bank" $ do
    it "deposits money" $ do
      runIdentity (execStateT (deposit 100) newBank) `shouldBe` [Deposit 100 firstOfJan2018]

    it "withdraws money" $ do
      runIdentity (execStateT (withdraw 100) newBank) `shouldBe` [Withdrawal 100 firstOfJan2018]

    it "prints a statement" $ do
      execWriter (evalStateT printStatement [Deposit 100 firstOfJan2018]) `shouldBe` "Deposited 100 | Balance 100\n"

    --it "prints a statement" $ do
      --execWriter (evalStateT doStatement newBank) `shouldBe` "Deposited 200 | Balance 200\nWithdrew 100 | Balance 100\nDesposited 3000 | Balance 3100\n"

instance MonadStatementPrinter (Writer String) where
  printSt = tell

instance MonadCurrentDateTime Identity where
  currentDateTime = pure firstOfJan2018

firstOfJan2018 = UTCTime (fromGregorian 2018 01 01) (secondsToDiffTime 0)

