import Test.Hspec

import Rover

main = hspec $ do
  describe "Rover" $ do
    it "should start at 0,0,N" $
      move "" `shouldBe` "0,0,N"

