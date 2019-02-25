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

doStatement :: TransactionRepo (Writer String) ()
doStatement = do
  deposit 200
  withdraw 100
  deposit 3000
  printStatement

newBank = []

spec :: Spec
spec = do
  describe "bank" $ do
    it "deposits money" $ do
      runIdentity (execStateT (deposit 100) newBank) `shouldBe` [Deposit 100 firstOfJan2018]

    it "withdraws money" $ do
      runIdentity (execStateT (withdraw 100) newBank) `shouldBe` [Withdrawal 100 firstOfJan2018]

    it "prints a deposit statement" $ do
      execWriter (evalStateT printStatement [Deposit 100 firstOfJan2018]) `shouldBe` "\
        \date       || credit || debit || balance\n\
        \01/01/2018 || 100.00 || || 100.00\n"

    it "prints a withdrawal statement" $ do
      execWriter (evalStateT printStatement [Withdrawal 100 firstOfJan2018]) `shouldBe` "\
        \date       || credit || debit || balance\n\
        \01/01/2018 || || 100.00 || -100.00\n"

    it "does all three" $ do
      execWriter (evalStateT doStatement newBank) `shouldBe` "\
        \date       || credit || debit || balance\n\
        \01/01/2018 || 200.00 || || 200.00\n\
        \01/01/2018 || || 100.00 || 100.00\n\
        \01/01/2018 || 3000.00 || || 3100.00\n"

instance MonadStatementPrinter (Writer String) where
  printSt = tell

instance MonadCurrentDateTime (Writer String) where
  currentDateTime = pure firstOfJan2018

instance MonadCurrentDateTime Identity where
  currentDateTime = pure firstOfJan2018

firstOfJan2018 = UTCTime (fromGregorian 2018 01 01) (secondsToDiffTime 0)

