module Game.Yahtzee.Yahtzee where

import Control.Monad.State
import System.Random

import qualified Data.Set as S
import qualified Data.Map as M

type Dice = Int

-- A roll is five dice
data Roll = Roll (Dice,Dice,Dice,Dice,Dice) deriving (Show)

-- Each time round, some of them can be held
data Hold = Hold (Bool,Bool,Bool,Bool,Bool) deriving (Show)

-- In the first roll, none are held.
initialHolds :: Hold
initialHolds = Hold (False,False,False,False,False)

-- TODO hide these constructors
data ScoreType = Aces
               | Twos
               | Threes
               | Fours
               | Fives
               | Sixes
               | ThreeOfAKind
               | FourOfAKind
               | FullHouse
               | SmallStraight
               | LargeStraight
               | FiveOfAKind
               | Chance
                 deriving (Show,Eq,Ord)

data GameState = GameState
  {
    gen :: StdGen
  , scoreCard :: M.Map ScoreType Roll
  , currentRoll :: Roll
  , holds :: Hold
  , available :: S.Set ScoreType
  } deriving (Show)

isUpper :: ScoreType -> Bool
isUpper = undefined

isLower :: ScoreType -> Bool
isLower = not . isLower

roll :: State GameState Roll
roll = do
  gs <- get
  let g = gen gs
      cRoll = currentRoll gs
      (a,g1) = randomR (1, 6) g
      (b,g2) = randomR (1, 6) g1
      (c,g3) = randomR (1, 6) g2
      (d,g4) = randomR (1, 6) g3
      (e,g5) = randomR (1, 6) g4
  put gs
         {
           gen = g5,
           currentRoll = updateVals (a,b,c,d,e) (holds gs) cRoll
         }
  gets currentRoll

updateVals :: (Int,Int,Int,Int,Int) -> Hold -> Roll -> Roll
updateVals (r1,r2,r3,r4,r5) (Hold (a,b,c,d,e)) (Roll (a1,a2,a3,a4,a5)) = Roll
                                (
                                  if a then a1 else r1,
                                  if b then a2 else r2,
                                  if c then a3 else r3,
                                  if d then a4 else r4,
                                  if e then a5 else r5
                                )

-- Given the set of available of score types and the current roll, pick the best
chooseScore :: S.Set ScoreType -> Roll -> ScoreType
chooseScore a _ = head (S.toList a)

-- Given the set of available score types, choose the ones to hold
chooseInitialHolds :: S.Set ScoreType -> Roll -> Hold
chooseInitialHolds _ _ = Hold (True,True,True,True,True)

-- Given the set of available score types, choose the ones to hold
chooseFinalHolds :: S.Set ScoreType -> Roll -> Hold
chooseFinalHolds _ _ = Hold (True,True,True,True,True)

updateHolds :: Hold -> State GameState ()
updateHolds h = modify (\x -> x { holds = h })

-- Update the score and reset the current roll
updateScore :: ScoreType -> Roll -> State GameState ()
updateScore s r = modify (\x -> x { scoreCard = M.insert s r (scoreCard x) })

{-
  How does Yahtzee work?

  There are 13 rounds

  Each round
    -- Roll dice
    -- choose holds
    -- Roll dice
    -- choose holds
    -- Roll dice
    -- Pick a ScoreType (cannot be reused)

-}

playRound :: State GameState (ScoreType,Roll)
playRound = do

  -- Available choices
  choices <- gets available
  
  -- Roll dice
  r <- roll 

  -- Choose holds
  let initialHolds' = chooseInitialHolds choices r
  modify (\x -> x { holds = initialHolds' })

  -- Roll dice
  r' <- roll 

  -- Choose final holds
  let subsequentHolds = chooseFinalHolds choices r'
  modify (\x -> x { holds = subsequentHolds })

  -- Last roll!
  r'' <- roll

  let score = chooseScore choices r''

  modify (\x -> x {
                    available = S.delete score choices
                  , holds = initialHolds
                  , scoreCard = M.insert score r'' (scoreCard x)
                  })
  
  return (score,r'')
  

initialState :: Int -> GameState
initialState seed  = GameState
  {
    gen = mkStdGen seed
  , scoreCard = M.empty 
  , currentRoll = Roll (1,1,1,1,1)
  , holds = initialHolds
  , available = S.fromList [Aces, Twos, Threes, Fours, Fives, Sixes,
                            ThreeOfAKind, FourOfAKind, FullHouse,
                            SmallStraight, LargeStraight, FiveOfAKind, Chance]
  }

runGame :: Int -> GameState
runGame seed = execState (replicateM 13 playRound) (initialState seed)

