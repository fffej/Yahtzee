module Main where

import Game.Yahtzee.Yahtzee
import Game.Yahtzee.Simulation
import Game.Yahtzee.Player

import Control.Monad.MC

{-
  This example defines a player who runs a monte carlo simulation
  After each player that rolls in order to maximise their score

  To do that, I evaluate the score 
-}

-- For each score type
--   Simulate rolls ahead x 1000, 

main :: IO ()
main = do
  print $ length $ filter (\(a,b) -> a > b) $ comparePlayers greedyPlayer daftPlayer 

