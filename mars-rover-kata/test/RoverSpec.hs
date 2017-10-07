import Test.Hspec

import Rover

main = hspec $ do
  describe "Rover" $ do
    it "should start at 0,0,N" $
      move "" `shouldBe` "0,0,N"

    it "should move north" $ do
      move "M" `shouldBe` "0,1,N"
      move "MM" `shouldBe` "0,2,N"
      move "MMMMM" `shouldBe` "0,5,N"

    it "should rotate left" $ do
      move "L" `shouldBe` "0,0,W"
      move "LL" `shouldBe` "0,0,S"
      move "LLL" `shouldBe` "0,0,E"
      move "LLLL" `shouldBe` "0,0,N"

    it "should rotate right" $ do
      move "R" `shouldBe` "0,0,E"
      move "RR" `shouldBe` "0,0,S"
      move "RRR" `shouldBe` "0,0,W"
      move "RRRR" `shouldBe` "0,0,N"

