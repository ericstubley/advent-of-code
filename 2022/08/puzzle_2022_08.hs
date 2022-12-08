module Puzzle_2022_08 where

import Automation (submitAnswer)
import Parsing
import Data.Char (digitToInt)
import Data.List (transpose)

-- data types

-- parsing
forestP :: Parser [[Int]]
forestP = sepBy (some (digitToInt <$> digitChar)) newline


-- functions
mark :: [Int] -> [Bool]
mark = mark' (-1)


mark' :: Int -> [Int] -> [Bool]
mark' _ [] = []
mark' height (t:trees)
    | t > height = True : mark' t trees
    | otherwise  = False : mark' height trees


left :: [[a]] -> [[a]]
left = id

right :: [[a]] -> [[a]]
right = map reverse

above :: [[a]] -> [[a]]
above = transpose

below :: [[a]] -> [[a]]
below = reverse . transpose . reverse


markVisible :: [[Int]] -> [[Bool]]
markVisible trees = foldl1 (zipWith (zipWith (||))) [markL, markR, markA, markB]
  where
    markL = map mark trees
    markR = right . (map mark) . right $ trees
    markA = above . (map mark) . above $ trees
    markB = below . (map mark) . below $ trees


countVisible :: [[Int]] -> Int
countVisible = length . (filter id) . concat . markVisible


views :: [Int] -> [Int]
views = views' 0 []

views' :: Int -> [(Int, Int)] -> [Int] -> [Int]
views' _ _ [] = []
views' distance [] (t:trees) = distance : views' 1 [(t, distance)] trees
views' distance (s:stack) (t:trees)
    | fst s < t  = views' (distance + snd s) stack (t:trees)
    | fst s > t  = distance : views' 1 ((t, distance) : s : stack) trees
    | fst s == t = distance : views' 1 ((t, distance + snd s) : stack) trees


scenicScores :: [[Int]] -> [[Int]]
scenicScores trees = foldl1 (zipWith (zipWith (*))) [viewL, viewR, viewA, viewB]
  where
    viewL = map views trees
    viewR = right . (map views) . right $ trees
    viewA = above . (map views) . above $ trees
    viewB = below . (map views) . below $ trees


mostScenic :: [[Int]] -> Int
mostScenic = maximum . concat . scenicScores

-- mains

mainA :: IO ()
mainA = do
    (Just forest) <- parseInput forestP "08/input.txt"
    let answer = countVisible forest
    print answer
    -- result <- submitAnswer 2022 08 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just forest) <- parseInput forestP "08/input.txt"
    let answer = mostScenic forest
    print answer
    -- result <- submitAnswer 2022 08 2 answer
    -- print result
    return ()
