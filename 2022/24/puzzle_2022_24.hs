module Puzzle_2022_24 where

import Automation (submitAnswer)
import Parsing
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.RWS
import Data.Massiv.Array (Array, Ix3(..), Ix2(..), IxN(..), (!>))
import qualified Data.Massiv.Array as A
import Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as Sq
import Data.Set (Set)
import qualified Data.Set as S
import Data.Foldable

-- data types
data Wind = Boreal | Vultur | Auster | Zephyr | Still deriving (Eq, Ord, Show)

type Array2 a = Array A.B Ix2 a
type Array3 a = Array A.U Ix3 a

data Storm = Storm
    { duration :: Int
    , height :: Int
    , width  :: Int
    , blizzards :: Array3 Bool}

-- parsing
stormP :: Parser Storm
stormP = do
    rows <- A.fromLists' A.Seq <$> between topP bottomP (some rowP)
    return $ buildStorm rows


topP :: Parser ()
topP = do
    char '#'
    char '.'
    some (char '#')
    newline
    char '#'
    return ()


bottomP :: Parser ()
bottomP = do
    some (char '#')
    char '.'
    char '#'
    return ()


rowP :: Parser [Wind]
rowP = some groundP <* (char '#' >> newline >> char '#')


groundP :: Parser Wind
groundP = (char '^' >> return Auster)
      <|> (char '>' >> return Zephyr)
      <|> (char 'v' >> return Boreal)
      <|> (char '<' >> return Vultur)
      <|> (char '.' >> return Still)


buildStorm :: Array2 Wind -> Storm
buildStorm winds = Storm range rows cols blizzards' where
    (A.Sz2 rows cols) = A.size winds
    range = lcm rows cols

    ixs = [(i :. j) | i <- [0..(rows-1)]
                    , j <- [0..(cols-1)]
                    , (winds A.! (i :. j)) /= Still]
    allPositions = S.unions $ map forecaster [0..(range-1)]

    forecaster :: Int -> Set Ix3
    forecaster t = S.fromList $ map (forecast t) ixs

    forecast :: Int -> Ix2 -> Ix3
    forecast t (r :. c) = 
        case (winds A.! (r :. c)) of
            Boreal -> (t :> mod (r + t) rows :. c)
            Vultur -> (t :> r :. mod (c - t) cols)
            Auster -> (t :> mod (r - t) rows :. c)
            Zephyr -> (t :> r :. mod (c + t) cols)

    constructor :: Ix3 -> Bool
    constructor ix = S.member ix allPositions

    blizzards' :: Array3 Bool
    blizzards' = A.makeArray A.Seq (A.Sz3 range rows cols) constructor


-- functions

findGoal :: Storm -> Int
findGoal storm = runReader (pathfinder (0 :> -1 :. 0) (endR :. endC)) storm
  where endR = (height storm)
        endC = (width storm) - 1
    

roundTrip :: Storm -> Int
roundTrip storm = t3
  where start = (-1 :. 0)
        end = (height storm :. width storm - 1)
        t1 = runReader (pathfinder (0 :> start) end) storm
        t2 = runReader (pathfinder (t1 :> end) start) storm
        t3 = runReader (pathfinder (t2 :> start) end) storm


-- if t >= best, return right away
-- otherwise check if at exit, update if better time
-- otherwise
pathfinder :: MonadReader Storm m => Ix3 -> Ix2 -> m Int
pathfinder start end = go (S.singleton start) (Sq.singleton start) where
    go :: MonadReader Storm m => Set Ix3 -> Seq Ix3 -> m Int
    go seen (ix :<| queue) = do
        let (t :> r :. c) = ix
        if (r :. c) == end then return t
            else do
                options <- (filterM inValley $ neighbours (t :> r :. c))
                           >>= filterM open
                           >>= filterM (unvisited seen)
                let seen' = S.union (S.fromList options) $ S.filter (current t) seen
                go seen' (queue >< (Sq.fromList options))


neighbours :: Ix3 -> [Ix3]
neighbours (t :> r :. c) = [ t+1 :> r+1 :. c
                           , t+1 :> r   :. c+1
                           , t+1 :> r   :. c
                           , t+1 :> r-1 :. c
                           , t+1 :> r   :. c-1]


inValley :: MonadReader Storm m => Ix3 -> m Bool
inValley (_ :> r :. c) = do
    rows <- reader height
    cols <- reader width
    let interior = 0 <= r && r < rows && 0 <= c && c < cols
    atEntrance <- (== (r :. c)) <$> entrance
    atExit <- (== (r :. c)) <$> exit
    return (interior || atEntrance || atExit)


entrance :: MonadReader Storm m => m Ix2
entrance = return (-1 :. 0)


exit :: MonadReader Storm m => m Ix2
exit = do
    rows <- reader height
    cols <- reader width
    return (rows :. cols - 1)


open :: MonadReader Storm m => Ix3 -> m Bool
open (t :> r :. c) = do
    atEntrance <- (== (r :. c)) <$> entrance
    atExit <- (== (r :. c)) <$> exit
    if atEntrance || atExit
        then return True
        else do
            t' <- reader $ (mod t) . duration
            reader $ not . (A.! (t' :> r :. c)) . blizzards


current :: Int -> Ix3 -> Bool
current mark (t :> _ :. _) = t >= mark


unvisited :: MonadReader Storm m => Set Ix3 -> Ix3 -> m Bool
unvisited seen (t :> r :. c) = do
--     t' <- reader $ (mod t) . duration
    return $ S.notMember (t :> r :. c) seen


-- for testing, extract positions at a given time
positions :: MonadReader Storm m => Int -> m [(Int, Int)]
positions t = do
    t' <- reader $ (mod t) . duration
    slice <- reader $ (!> t') . blizzards
    rows <- reader height
    cols <- reader width
    let ixs = [(i, j) | i <- [0..(rows-1)]
                      , j <- [0..(cols-1)]
                      , slice A.! (i :. j)]
    return ixs


-- mains

mainA :: IO ()
mainA = do
    (Just storm) <- parseInput stormP "24/input.txt"
    let answer = findGoal storm
    print answer
    -- result <- submitAnswer 2022 24 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just storm) <- parseInput stormP "24/input.txt"
    let answer = roundTrip storm
    print answer
    result <- submitAnswer 2022 24 2 answer
    print result
    return ()
