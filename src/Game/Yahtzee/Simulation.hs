module Game.Yahtzee.Simulation where

import Game.Yahtzee.Yahtzee
import Game.Yahtzee.Player

comparePlayers :: Player -> Player -> [(Int,Int)]
comparePlayers p1 p2 = map (\x -> (finalScore x p1, finalScore x p2)) [1..10000]
