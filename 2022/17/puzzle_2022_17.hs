module Puzzle_2022_17 where

import Automation (submitAnswer)
import Parsing
import Control.Monad.State
import Control.Applicative (liftA2)
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Lens.Micro.Platform

-- data types
data Jet = L | R | Flag
    deriving (Eq, Ord, Show)

data Rock = Horizontal | Cross | Bend | Vertical | Square 
    deriving (Eq, Ord, Show, Bounded, Enum)

type Coord = (Int, Int)

type Rockpile = Set Coord

data Chamber = Chamber
    { _rockpile :: Rockpile
    , _rocks :: [Rock]
    , _jets :: [Jet]
    , _current :: Coord}
        deriving (Eq, Ord, Show)

makeLenses ''Chamber

-- parsing
jetsP :: Parser [Jet]
jetsP = many $ (char '>' >> return R) <|> (char '<' >> return L)


-- functions
width :: Int
width = 7


shift :: Coord -> Coord -> Coord
shift (oh, ox) (h, x) = (oh + h, ox + x)


baseMap :: Rock -> Rockpile
baseMap rock = 
    case rock of
        Horizontal -> S.fromList [(0, 0), (0, 1), (0, 2), (0, 3)]
        Cross      -> S.fromList [(0, 1), (1, 0), (1, 1), (1, 2), (2, 1)]
        Bend       -> S.fromList [(0, 0), (0, 1), (0, 2), (1, 2), (2, 2)]
        Vertical   -> S.fromList [(0, 0), (1, 0), (2, 0), (3, 0)]
        Square     -> S.fromList [(0, 0), (0, 1), (1, 0), (1, 1)]


rockMap :: Rock -> Coord -> Rockpile
rockMap rock offset = S.map (shift offset) (baseMap rock)



initialChamber :: [Jet] -> Chamber
initialChamber j = Chamber rp rs js c where
    rp = S.fromList $ map (\x -> (0, x)) [0..(width-1)]
    rs = cycle [minBound::Rock .. maxBound::Rock]
    js = cycle (j ++ [Flag])
    c = (3, 2)


height :: MonadState Chamber m => m Int
height = gets $ fst . S.findMax . (^. rockpile)


spawn :: MonadState Chamber m => m ()
spawn = do
    h <- height
    modify $ set current (h+4, 2)


push :: MonadState Chamber m => m ()
push = do
    ch <- gets (^. rockpile)
    r <- gets $ head . (^. rocks)
    j <- gets $ head . (^. jets)
    (h, x) <- gets (^. current)
    modify $ over jets tail
    let rp = rockMap r (h, x)
    let rp' = applyJet rp j
    let jv = case j of
                L -> (0, -1)
                R -> (0, 1)
    if (inBounds rp' && S.disjoint rp' ch)
        then modify $ set current (shift (h, x) jv)
        else return ()


applyJet :: Rockpile -> Jet -> Rockpile
applyJet rp j = case j of
                    L -> S.map (shift (0, -1)) rp
                    R -> S.map (shift (0, 1)) rp


inBounds :: Rockpile -> Bool
inBounds rp = (S.findMin xs >= 0) && (S.findMax xs < width) where
    xs = S.map snd rp


down :: MonadState Chamber m => m Bool
down = do
    ch <- gets (^. rockpile)
    r <- gets $ head . (^. rocks)
    (h, x) <- gets (^. current)
    let rp = rockMap r (h, x)
    let rp' = rockMap r (h-1, x)
    if (S.disjoint rp' ch)
        then do
            modify $ set current (h-1, x)
            return True
        else do
            modify $ over rocks tail
            modify $ set rockpile (S.union rp ch)
            return False


dropRock :: MonadState Chamber m => m ()
dropRock = spawn >> (repeatUntilM (clearFlag >> push >> down))


computeHeight :: MonadState Chamber m => Int -> m Int
computeHeight n = repeatM n dropRock >> height


-- from johan hidding's aoc 2021 code that I often refer to
repeatM :: Applicative m => Int -> m a -> m [a]
repeatM n action = loop n where
    loop n
        | n <= 0    = pure []
        | otherwise = liftA2 (:) action (loop (n-1))


-- from johan hidding's aoc 2021 code that I often refer to
countRepeatUntilM :: (Monad m) => m Bool -> m Int
countRepeatUntilM action = go 1 where 
    go n = do
        stop <- action
        if stop 
            then return n 
            else go (n + 1)


repeatUntilM :: Monad m => m Bool -> m ()
repeatUntilM action = do
    result <- action
    if result
        then repeatUntilM action
        else return ()



heightAfter :: [Jet] -> Int -> Int
heightAfter js n = evalState (computeHeight n) (initialChamber js)


detectCycle :: MonadState Chamber m => m Bool
detectCycle = do
    r <- gets $ head . (^. rocks)
    j <- gets $ head . (^. jets)
    return $ r == (minBound :: Rock) && j == Flag


cycleLength :: MonadState Chamber m => m Int
cycleLength = countRepeatUntilM (dropRock >> detectCycle)


clearFlag :: MonadState Chamber m => m ()
clearFlag = modify $ over jets (dropWhile (== Flag))


heightAfterLong :: [Jet] -> Int -> Int
heightAfterLong js n = cycleHeight * cycles + remainderHeight where
    ic = initialChamber js
    ic' = set rocks (cycle [Horizontal]) ic
    period = evalState cycleLength ic'
    (cycles, remainder) = divMod n period
    cycleHeight = evalState (computeHeight period) ic'
    remainderHeight = evalState (computeHeight remainder) ic'

-- mains

mainA :: IO ()
mainA = do
    (Just jets) <- parseInput jetsP "17/input.txt"
    let answer = heightAfter jets 2022
    print answer
    result <- submitAnswer 2022 17 1 answer
    print result
    return ()


mainB :: IO ()
mainB = do
    let answer = 0
    print answer
    -- result <- submitAnswer 2022 17 2 answer
    -- print result
    return ()
