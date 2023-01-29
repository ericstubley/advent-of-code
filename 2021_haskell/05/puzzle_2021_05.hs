module Puzzle_2021_05 where

import Automation (submitAnswer)
import Parsing
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

-- data types
type Point = (Int, Int)
type Segment = (Point, Point)


data Count = One | Many deriving (Eq, Ord, Show)

instance Semigroup Count where
    Many <> x = Many
    x <> Many = Many
    One <> One = Many 

-- parsing
segmentsP :: Parser [Segment]
segmentsP = sepBy segmentP newline


segmentP :: Parser Segment
segmentP = (,) <$> pointP <* (string " -> ") <*> pointP


pointP :: Parser Point
pointP = (,) <$> integer <* (char ',') <*> integer


-- functions
points :: Segment -> [Point]
points s = [ (ax + i*dx, ay + i*dy) | i <- [0..t]]
  where ((ax, ay), (bx, by)) = s
        (dx, dy) = direction s
        t = distance s


direction :: Segment -> Point
direction ((ax, ay), (bx, by)) = (sgn (bx - ax), sgn (by - ay))


distance :: Segment -> Int
distance ((ax, ay), (bx, by)) = max (abs (bx - ax)) (abs (by - ay))


sgn :: Int -> Int
sgn x
    | x == 0 = 0
    | x >  0 = 1
    | x <  0 = -1


diagonal :: Segment -> Bool
diagonal s = (1, 1) == (abs dx, abs dy)
  where (dx, dy) = direction s


overlapsA :: [Segment] -> Int
overlapsA segments = overlaps . filter (not . diagonal) $ segments


overlapsB :: [Segment] -> Int
overlapsB = overlaps


overlaps :: [Segment] -> Int
overlaps segments = length . filter (== Many) . M.elems $ counts
  where counts = M.unionsWith (<>) (map buildMap segments)


buildMap :: Segment -> Map Point Count
buildMap segment = M.fromList $ zip (points segment) (repeat One)


-- mains

mainA :: IO ()
mainA = do
    (Just segments) <- parseInput segmentsP "05/input.txt"
    let answer = overlapsA segments
    print answer
    -- result <- submitAnswer 2021 05 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just segments) <- parseInput segmentsP "05/input.txt"
    let answer = overlapsB segments
    print answer
    -- result <- submitAnswer 2021 05 2 answer
    -- print result
    return ()
