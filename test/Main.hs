module Main (main) where

import Test.Hspec

import Game.Yahtzee.Yahtzee

toDie :: [Int] -> [Dice]
toDie xs = map (\x -> toEnum (x - 1)) xs

yahtzeeSpecs :: Spec
yahtzeeSpecs = do
  describe "Yahtzee rules for " $ do
    it "scoring for single roles" $ do
      scoreRoll (toDie [1,1,1,1,1]) Ones    `shouldBe` 5
      scoreRoll (toDie [2,2,2,2,2]) Ones    `shouldBe` 0
      scoreRoll (toDie [2,2,2,2,2]) Twos    `shouldBe` 10
      scoreRoll (toDie [1,2,3,4,5]) Chance  `shouldBe` 15
    it "scoring for three, four and five of a kind" $ do
      scoreRoll (toDie [5,2,5,4,5]) ThreeOfAKind `shouldBe` 21
      scoreRoll (toDie [5,2,1,4,5]) ThreeOfAKind `shouldBe` 0
      scoreRoll (toDie [5,2,5,4,5]) FourOfAKind `shouldBe` 0
      scoreRoll (toDie [5,5,1,5,5]) FourOfAKind `shouldBe` 21
      scoreRoll (toDie [1,1,1,1,1]) FiveOfAKind `shouldBe` 50
      scoreRoll (toDie [2,2,2,2,2]) FiveOfAKind `shouldBe` 50
    

main :: IO ()
main = hspec $ do
  yahtzeeSpecs
