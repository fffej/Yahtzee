module Main (main) where

import Test.Hspec

import Game.Yahtzee.Yahtzee

yahtzeeSpecs :: Spec
yahtzeeSpecs = do
  describe "Yahtzee" $ do
    it "" $ do
      (1 + 1) `shouldBe` 2
    

main :: IO ()
main = hspec $ do
  yahtzeeSpecs
