module Game.Yahtzee.Player where

import Game.Yahtzee.Yahtzee

import Data.Ord (comparing)
import Data.List

holdAll :: ScoreCard -> Roll -> Hold
holdAll _ _ = [True,True,True,True,True]

holdContributors :: ScoreCard -> Roll -> Hold
holdContributors sc r = holdGoal goal r
  where
    goal = chooseBest sc r

holdGoal = undefined

chooseFirst :: ScoreCard -> a -> ScoreType
chooseFirst a _ = head (free a)

chooseBest :: ScoreCard -> Roll -> ScoreType
chooseBest a b = maximumBy (comparing (scoreRoll b)) (free a)

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

{-
  The better player
  -- chooses the best roll based on the average score they'll get
  -- holds only the things that contribute to the roll and tries again
-}
betterPlayer :: Player
betterPlayer = Player
  {
    chooseScore = chooseBest
  , chooseInitialHolds = holdContributors
  , chooseFinalHolds = holdContributors
  }


