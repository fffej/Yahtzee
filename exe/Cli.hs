module Main where

import Game.Yahtzee.Yahtzee
import Game.Yahtzee.Simulation
import Game.Yahtzee.Player

main :: IO ()
main = do
  print $ length $ filter (\(a,b) -> a > b) $ comparePlayers greedyPlayer daftPlayer 

