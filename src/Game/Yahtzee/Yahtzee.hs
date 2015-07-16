module Game.Yahtzee.Yahtzee where

import Data.List
import Control.Monad.State
import System.Random
import Data.Ord

import qualified Data.Set as S
import qualified Data.Map as M

data Dice = One | Two | Three | Four | Five | Six deriving (Eq,Ord,Show,Enum,Bounded)

instance Random Dice where
  randomR (a,b) g = (toEnum x, g')
    where
      (x,g') = randomR (fromEnum a, fromEnum b) g
  random = randomR (minBound, maxBound)

-- A roll is five dice
data Roll = Roll (Dice,Dice,Dice,Dice,Dice) deriving (Show)

-- Each time round, some of them can be held
data Hold = Hold (Bool,Bool,Bool,Bool,Bool) deriving (Show)

-- A player has three jobs.  Choose what to hold, and finally choose a score
data Player = Player
              {
                chooseInitialHolds :: S.Set ScoreType -> Roll -> Hold
              , chooseFinalHolds :: S.Set ScoreType -> Roll -> Hold
              , chooseScore :: S.Set ScoreType -> Roll -> ScoreType
              }

-- In the first roll, none are held.
initialHolds :: Hold
initialHolds = Hold (False,False,False,False,False)

data ScoreType = Ones
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
  , player :: Player
  } 

isUpper :: ScoreType -> Bool
isUpper Ones   = True
isUpper Twos   = True
isUpper Threes = True
isUpper Fours  = True
isUpper Fives  = True
isUpper Sixes  = True
isUpper _      = False

isLower :: ScoreType -> Bool
isLower = not . isUpper

rollDie :: StdGen -> Int -> ([Dice],StdGen)
rollDie g 0 = ([], g)
rollDie g n = (x : y, g'')
  where
    (x,g') = random g
    (y,g'') = rollDie g' (n - 1)

roll :: State GameState Roll
roll = do
  gs <- get
  let g = gen gs
      cRoll = currentRoll gs
      (a:b:c:d:e:[], g') = rollDie g 5
  put gs
         {
           gen = g',
           currentRoll = updateVals (a,b,c,d,e) (holds gs) cRoll
         }
  gets currentRoll

updateVals :: (Dice,Dice,Dice,Dice,Dice) -> Hold -> Roll -> Roll
updateVals (r1,r2,r3,r4,r5) (Hold (a,b,c,d,e)) (Roll (a1,a2,a3,a4,a5)) = Roll
                                (
                                  if a then a1 else r1,
                                  if b then a2 else r2,
                                  if c then a3 else r3,
                                  if d then a4 else r4,
                                  if e then a5 else r5
                                )

updateHolds :: Hold -> State GameState ()
updateHolds h = modify (\x -> x { holds = h })

updateScore :: ScoreType -> Roll -> State GameState ()
updateScore s r = modify (\x -> x { scoreCard = M.insert s r (scoreCard x) })

reroll :: Hold -> State GameState Roll
reroll holds' = do
  modify (\x -> x { holds = holds' })
  roll

playRound :: State GameState (ScoreType,Roll)
playRound = do
  choices <- gets available
  p <- gets player
  r <- roll 
  r' <- reroll (chooseInitialHolds p choices r)
  r'' <- reroll (chooseFinalHolds p choices r')
  let score = chooseScore p choices r''
  modify (\x -> x {
                    available = S.delete score choices
                  , holds = initialHolds
                  , scoreCard = M.insert score r'' (scoreCard x)
                  })
  
  return (score,r'')
  

initialState :: Int -> Player -> GameState
initialState seed p = GameState
  {
    gen = mkStdGen seed
  , scoreCard = M.empty 
  , currentRoll = Roll (One,One,One,One,One)
  , holds = initialHolds
  , available = S.fromList [Ones, Twos, Threes, Fours, Fives, Sixes,
                            ThreeOfAKind, FourOfAKind, FullHouse,
                            SmallStraight, LargeStraight, FiveOfAKind, Chance]
  , player = p
  }

finalScore :: Int -> Player -> Int
finalScore seed p = (scoreGame . scoreCard) (runGame seed p)

runGame :: Int -> Player -> GameState
runGame seed p = execState (replicateM 13 playRound) (initialState seed p)

scoreGame :: M.Map ScoreType Roll -> Int
scoreGame xs = scoreRolls lower + upperScore + upperBonus
  where
    upperScore = scoreRolls upper
    upperBonus = if upperScore >= 63 then 35 else 0
    (lower,upper) = M.partitionWithKey (\st _ -> isLower st) xs
    scoreRolls = M.foldrWithKey (\st r s -> s + scoreRoll (toList r) st) 0 

toInt :: Dice -> Int
toInt = (+ 1) . fromEnum

toList :: Roll -> [Dice]
toList (Roll (a,b,c,d,e)) = [a,b,c,d,e]

groupDie :: [Dice] -> [[Dice]]
groupDie = group . sort

maxEqualDie :: [Dice] -> Int
maxEqualDie r = length (maximumBy (comparing length) $ groupDie r)

isFullHouse :: [Dice] -> Bool
isFullHouse r = length (filter ((== 3) . length) groupedDie) == 1 &&
                length (filter ((== 2) . length) groupedDie) == 1
  where
    groupedDie = groupDie r

contiguous :: [Int] -> Bool
contiguous xs = all (\(x,y) -> x + 1 == y) $ zip s (tail s)
  where
    s = sort xs

isLargeStraight :: [Dice] -> Bool
isLargeStraight = contiguous . map toInt

isSmallStraight :: [Dice] -> Bool
isSmallStraight r = any contiguous listsOf4
  where
    listsOf4 = filter ((== 4) . length) (subsequences (map toInt r))

scoreRoll :: [Dice] -> ScoreType -> Int
scoreRoll r Ones   = 1 * length (filter (== One) r)
scoreRoll r Twos   = 2 * length (filter (== Two) r)
scoreRoll r Threes = 3 * length (filter (== Three) r)
scoreRoll r Fours  = 4 * length (filter (== Four) r)
scoreRoll r Fives  = 5 * length (filter (== Five) r)
scoreRoll r Sixes  = 6 * length (filter (== Six) r)
scoreRoll r Chance = sum (map toInt r)
scoreRoll r ThreeOfAKind
  | maxEqualDie r >= 3 = sum (map toInt r)
  | otherwise          = 0
scoreRoll r FourOfAKind
  | maxEqualDie r >= 4 = sum (map toInt r)
  | otherwise          = 0
scoreRoll r FiveOfAKind
  | maxEqualDie r == 5 = 50
  | otherwise          = 0
scoreRoll r FullHouse
  | isFullHouse r = 25
  | otherwise     = 0
scoreRoll r SmallStraight
  | isSmallStraight r = 30
  | otherwise = 0
scoreRoll r LargeStraight
  | isLargeStraight r = 40
  | otherwise = 0
