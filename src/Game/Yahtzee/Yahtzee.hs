module Game.Yahtzee.Yahtzee where

import Data.List
import Control.Monad.State
import System.Random
import Data.Ord
import Data.Maybe (isNothing)

import qualified Data.Map as M

data Dice = One | Two | Three | Four | Five | Six deriving (Eq,Ord,Show,Enum,Bounded)

instance Random Dice where
  randomR (a,b) g = (toEnum x, g')
    where
      (x,g') = randomR (fromEnum a, fromEnum b) g
  random = randomR (minBound, maxBound)

-- A roll is list of dice
type Roll = [Dice] 

-- Each time round, some of them can be held
type Hold = [Bool]

-- A player has three jobs.  Choose what to hold, and finally choose a score
data Player = Player
              {
                chooseInitialHolds ::  ScoreCard -> Roll -> Hold
              , chooseFinalHolds :: ScoreCard -> Roll -> Hold
              , chooseScore :: ScoreCard -> Roll -> ScoreType
              }

-- In the first roll, none are held.
initialHolds :: Hold
initialHolds = [False,False,False,False,False]

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

type ScoreCard = M.Map ScoreType (Maybe Roll)

data GameState = GameState
  {
    gen :: StdGen
  , scoreCard :: ScoreCard
  , currentRoll :: Roll
  , holds :: Hold
  , player :: Player
  }

available :: GameState -> [ScoreType]
available = free . scoreCard

free :: ScoreCard -> [ScoreType]
free = M.keys . M.filter isNothing

isUpper :: ScoreType -> Bool
isUpper = flip elem [Ones,Twos,Threes,Fours,Fives,Sixes]

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
      (dies, g') = rollDie g 5
  put gs
         {
           gen = g',
           currentRoll = updateVals dies (holds gs) cRoll
         }
  gets currentRoll

updateVals :: Roll -> Hold -> Roll -> Roll
updateVals r h n = zipWith3 (\x y z -> if x then y else z) h r n

reroll :: Hold -> State GameState Roll
reroll holds' = modify (\x -> x { holds = holds' }) >> roll

playRound :: State GameState (ScoreType,Roll)
playRound = do
  choices <- gets scoreCard
  p <- gets player
  r <- roll 
  r' <- reroll (chooseInitialHolds p choices r)
  r'' <- reroll (chooseFinalHolds p choices r')
  let score = chooseScore p choices r''
  modify (\x -> x {
                    holds = initialHolds
                  , scoreCard = M.insert score (Just r'') (scoreCard x)
                  })
  
  return (score,r'')
  
initialState :: Int -> Player -> GameState
initialState seed p = GameState
  {
    gen = mkStdGen seed
  , currentRoll = [One,One,One,One,One]
  , holds = initialHolds
  , scoreCard = M.fromList $
                zip [Ones, Twos, Threes, Fours, Fives, Sixes,
                     ThreeOfAKind, FourOfAKind, FullHouse,
                     SmallStraight, LargeStraight, FiveOfAKind, Chance] $
                repeat Nothing 
  , player = p
  }

finalScore :: Int -> Player -> Int
finalScore seed p = (scoreGame . scoreCard) (runGame seed p)

runGame :: Int -> Player -> GameState
runGame seed p = execState (replicateM 13 playRound) (initialState seed p)

scoreGame :: ScoreCard -> Int
scoreGame ys = scoreRolls lower + upperScore + upperBonus
  where
    xs = M.mapWithKey (\_ (Just y) -> y) ys -- TODO
    upperScore = scoreRolls upper
    upperBonus = if upperScore >= 63 then 35 else 0
    (lower,upper) = M.partitionWithKey (\st _ -> isLower st) xs
    scoreRolls = M.foldrWithKey (\st r s -> s + scoreRoll r st) 0 

toInt :: Dice -> Int
toInt = (+ 1) . fromEnum

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

scoreLower :: Dice -> [Dice] -> Int
scoreLower n r = (toInt n) * length (filter (== n) r)

scoreRoll :: [Dice] -> ScoreType -> Int
scoreRoll r Ones   = scoreLower One r
scoreRoll r Twos   = scoreLower Two r
scoreRoll r Threes = scoreLower Three r
scoreRoll r Fours  = scoreLower Four r
scoreRoll r Fives  = scoreLower Five r 
scoreRoll r Sixes  = scoreLower Six r
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
