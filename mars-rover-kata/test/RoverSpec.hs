import Test.Hspec

import Rover

moveNoObstacles :: String -> String
moveNoObstacles commands = move commands []

main = hspec $ do
  describe "Rover" $ do
    it "should start at 0,0,N" $
      moveNoObstacles "" `shouldBe` "0,0,N"

    it "should move north" $ do
      moveNoObstacles "M" `shouldBe` "0,1,N"
      moveNoObstacles "MM" `shouldBe` "0,2,N"
      moveNoObstacles "MMMMM" `shouldBe` "0,5,N"

    it "should rotate left" $ do
      moveNoObstacles "L" `shouldBe` "0,0,W"
      moveNoObstacles "LL" `shouldBe` "0,0,S"
      moveNoObstacles "LLL" `shouldBe` "0,0,E"
      moveNoObstacles "LLLL" `shouldBe` "0,0,N"

    it "should rotate right" $ do
      moveNoObstacles "R" `shouldBe` "0,0,E"
      moveNoObstacles "RR" `shouldBe` "0,0,S"
      moveNoObstacles "RRR" `shouldBe` "0,0,W"
      moveNoObstacles "RRRR" `shouldBe` "0,0,N"

    it "should wrap around when hitting the end of the grid" $ do
      moveNoObstacles "MMMMMMMMMM" `shouldBe` "0,0,N"

    it "should move east" $ do
      moveNoObstacles "RM" `shouldBe` "1,0,E"
      moveNoObstacles "RMMMMMMMMMM" `shouldBe` "0,0,E"

    it "should move west" $ do
      moveNoObstacles "LM" `shouldBe` "9,0,W"

    it "should move south" $ do
      moveNoObstacles "LLM" `shouldBe` "0,9,S"

    it "stops when it hits an obstacle" $ do
      move "MMM" [(0,2)] `shouldBe` "O,0,1,N"
      move "MMRMMMRM" [(3,1)] `shouldBe` "O,3,2,S"

