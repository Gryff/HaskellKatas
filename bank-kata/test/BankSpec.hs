module BankSpec where

import Test.Hspec
import Test.QuickCheck

import Bank

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "bank" $ do
    it "deposits money" $ do
      deposit (Deposit 100) (Bank 0) `shouldBe` (Bank 100)

