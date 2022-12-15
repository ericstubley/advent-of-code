module Puzzle_2022_15 where

import Automation (submitAnswer)
import Parsing
import Data.Massiv.Array (Ix2(..))
import Data.List (nub, partition, span)

-- data types
-- don't be fooled by tuple notation! we're using this for closed intervals
type Interval = (Int, Int)

-- parsing
pairsP :: Parser [(Ix2, Ix2)]
pairsP = sepBy pairP newline


-- Sensor at x=2, y=18: closest beacon is at x=-2, y=15
pairP :: Parser (Ix2, Ix2)
pairP = do
    string "Sensor at x="
    sx <- integer
    string ", y="
    sy <- integer
    string ": closest beacon is at x="
    bx <- integer
    string ", y="
    by <- integer
    return (sx :. sy, bx :. by)


-- functions

manhattan :: Ix2 -> Ix2 -> Int
manhattan (x1 :. y1) (x2 :. y2) = abs (x1 - x2) + abs (y1 - y2)


member :: Interval -> Int -> Bool
member (l, u) x = l <= x && x <= u


overlap :: Interval -> Interval -> Bool
overlap (l1, u1) (l2, u2) = member (l1, u1+1) l2
                         || member (l1-1, u1) u2
                         || member (l2, u2+1) l1
                         || member (l2-1, u2) u1


-- assumes they overlap
union :: Interval -> Interval -> Interval
union (l1, u1) (l2, u2) = (min l1 l2, max u1 u2)


width :: Interval -> Int
width (l, u) = u - l + 1


nonempty :: Interval -> Bool
nonempty (l, u) = l <= u


countCoverage :: Int -> [(Ix2, Ix2)] -> Int
countCoverage row pairs = covered - beacons
  where
    covered = sum . map width $ coverage row pairs
    beacons = length . nub . filter (inRow row) . map snd $ pairs


inRow :: Int -> Ix2 -> Bool
inRow row (x :. y) = y == row


coverage :: Int -> [(Ix2, Ix2)] -> [Interval]
coverage row pairs = foldl merge [] contributions
  where
    contributions = filter nonempty . map (coverageInterval row) $ pairs


coverageInterval :: Int -> (Ix2, Ix2) -> Interval
coverageInterval row (sensor, beacon) = (sx - discrepancy, sx + discrepancy)
  where (sx :. sy) = sensor
        discrepancy = manhattan sensor beacon - manhattan sensor (sx :. row)


merge :: [Interval] -> Interval -> [Interval]
merge is i = merged : disjoint where
    (overlapping, disjoint) = partition (overlap i) is
    merged = foldl1 union (i : overlapping)


tuningFrequency :: Ix2 -> Int
tuningFrequency (x :. y) = 4000000*x + y


find :: Int -> Int -> [(Ix2, Ix2)] -> Ix2
find lower upper pairs = (fx :. fy) where
    rows = map (\r -> coverage r pairs) [lower..upper]
    rows' = map (clip lower upper) rows
    (prefix, remainder) = span (== [(lower, upper)]) rows'
    row = head remainder
    fy = length prefix
    fx = narrow lower upper row


-- given a list of intervals with exactly one gap, find the gap
-- should only ever be a single interval
narrow :: Int -> Int -> [Interval] -> Int
narrow lower upper ((l, u):[])
    | lower /= l = u
    | upper /= u = l
narrow lower upper ((l1, u1):(l2, u2):[]) = (min u1 u2) + 1
narrow lower upper _ = error "wrong length"




clip :: Int -> Int -> [Interval] -> [Interval]
clip lower upper [] = []
clip lower upper ((l, u):is)
    | u < lower || l > upper = clip lower upper is
    | otherwise              = (max lower l, min upper u) : clip lower upper is


-- mains

mainA :: IO ()
mainA = do
    (Just pairs) <- parseInput pairsP "15/input.txt"
    let answer = countCoverage 2000000 pairs
    print answer
    -- result <- submitAnswer 2022 15 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just pairs) <- parseInput pairsP "15/input.txt"
    let answer = tuningFrequency $ find 0 4000000 pairs
    print answer
    result <- submitAnswer 2022 15 2 answer
    print result
    return ()
