module Puzzle_2019_03 where


import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Automation (submitAnswer)
import Parsing

-- data types
type Point = (Int, Int)
data Direction = U | R | D | L deriving (Eq, Show)
data Segment = Segment {dir :: Direction, len :: Int} deriving (Eq, Show)
type Wire = [Segment]


-- parsing
wiresP :: Parser (Wire, Wire)
wiresP = do
    w1 <- wireP
    newline
    w2 <- wireP
    return (w1, w2)


wireP :: Parser Wire
wireP = sepBy segmentP (char ',')


segmentP :: Parser Segment
segmentP = do
    d <- dirP
    l <- integer
    return $ Segment d l


dirP :: Parser Direction
dirP = uP <|> rP <|> dP <|> lP where
    uP = char 'U' >> return U
    rP = char 'R' >> return R
    dP = char 'D' >> return D
    lP = char 'L' >> return L


-- functions
manhattan :: Point -> Point -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)


directionVector :: Direction -> Point
directionVector d = case d of
    U -> (0, 1)
    R -> (1, 0)
    D -> (0, -1)
    L -> (-1, 0)


wirePoints :: Wire -> [Point]
wirePoints w = snd $ foldl builder ((0, 0), []) w
  where
    builder :: (Point, [Point]) -> Segment -> (Point, [Point])
    builder (start, acc) segment = ( segmentEnd start segment
                                   , acc ++ segmentPoints start segment)


wireSet :: Wire -> Set Point
wireSet w = S.fromList $ wirePoints w


wireMap :: Wire -> Map Point Int
wireMap w = M.fromListWith min $ zip (wirePoints w) [1..]


segmentPoints :: Point -> Segment -> [Point]
segmentPoints (x0, y0) segment = [(x0 + i*dx, y0 + i*dy) | i <- [1..l]]
  where (dx, dy) = directionVector $ dir segment
        l = len segment


segmentEnd :: Point -> Segment -> Point
segmentEnd (x0, y0) segment = (x0 + l*dx, y0 + l*dy)
  where (dx, dy) = directionVector $ dir segment
        l = len segment


intersections :: Wire -> Wire -> [Point]
intersections w1 w2 = S.toList $ S.intersection (wireSet w1) (wireSet w2)


closestIntersection :: Wire -> Wire -> Int
closestIntersection w1 w2 = minimum $ map distToOrigin intersects
  where distToOrigin = manhattan (0, 0)
        intersects = intersections w1 w2


minimalDelay :: Wire -> Wire -> Int
minimalDelay w1 w2 = minimum . M.elems $ M.intersectionWith (+) (wireMap w1) (wireMap w2)


-- mains

mainA :: IO ()
mainA = do
    Just (a, b) <- parseInput wiresP "03/input.txt"
    let answer = closestIntersection a b
    print answer
    -- result <- submitAnswer 2019 03 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    Just (a, b) <- parseInput wiresP "03/input.txt"
    let answer = minimalDelay a b
    print answer
    -- result <- submitAnswer 2019 03 2 answer
    -- print result
    return ()
