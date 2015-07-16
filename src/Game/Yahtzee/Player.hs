module Game.Yahtzee.Player where

import Game.Yahtzee.Yahtzee

import qualified Data.Set as S
import Data.Ord (comparing)
import Data.List

holdAll :: a -> b -> Hold
holdAll _ _ = Hold(True,True,True,True,True)

chooseFirst :: S.Set ScoreType -> a -> ScoreType
chooseFirst a _ = head (S.toList a)

chooseBest :: S.Set ScoreType -> Roll -> ScoreType
chooseBest a b = maximumBy (comparing (scoreRoll (toList b))) $ S.toList a

{-
   The daft player
   -- never rerolls his dice
   -- fills in his score sheet in order
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
  -- chooses the best based on the hand he has and maximising the score
-}
greedyPlayer :: Player
greedyPlayer = Player
  {
    chooseScore = chooseBest
  , chooseInitialHolds = holdAll
  , chooseFinalHolds = holdAll
  }
