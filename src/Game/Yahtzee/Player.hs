module Game.Yahtzee.Player where

import Game.Yahtzee.Yahtzee

import Data.Ord (comparing)
import Data.List

holdAll :: a -> b -> Hold
holdAll _ _ = Hold(True,True,True,True,True)

chooseFirst :: [ScoreType] -> a -> ScoreType
chooseFirst a _ = head a

chooseBest :: [ScoreType] -> Roll -> ScoreType
chooseBest a b = maximumBy (comparing (scoreRoll (toList b))) a

{-
   The daft player
   -- never rerolls his dice
   -- fills in the score sheet in order
-}
daftPlayer :: Player
daftPlayer = Player
  {
    chooseScore = chooseFirst
  , chooseInitialHolds = holdAll
  , chooseFinalHolds = holdAll
  }

{-
  The greedy player
  -- chooses the best based maximising the score of the current roll
-}
greedyPlayer :: Player
greedyPlayer = Player
  {
    chooseScore = chooseBest
  , chooseInitialHolds = holdAll
  , chooseFinalHolds = holdAll
  }
