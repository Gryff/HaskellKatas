module BankSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Monad.State

import Bank

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

doDeposit amount = do
  deposit amount

doWithdrawal amount = do
  withdraw amount

spec :: Spec
spec = do
  describe "bank" $ do
    it "deposits money" $ do
      runState (doDeposit 100) newBank `shouldBe` ((), [Deposit 100])

    it "withdraws money" $ do
      runState (doWithdrawal 100) newBank `shouldBe` ((), [Withdrawal 100])

