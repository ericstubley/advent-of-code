module Puzzle_2023_09 where

import Automation (submitAnswer)
import Parsing

-- data types

-- parsing
reportP :: Parser [[Int]]
reportP = sepBy historyP newline

historyP :: Parser [Int]
historyP = sepBy integer (char ' ')

-- functions
-- prediction :: [Int] -> Int
-- prediction xs
--     | all (==0) xs = 0
--     | otherwise    = last xs + (prediction $ diffs xs)

-- extrapolation :: [Int] -> Int
-- extrapolation = prediction . reverse

-- bc Haskell lists, use the version that uses head rather than last
extrapolation :: [Int] -> Int
extrapolation xs
    | all (==0) xs = 0
    | otherwise    = head xs - (extrapolation $ diffs xs)

prediction :: [Int] -> Int
prediction = extrapolation . reverse

diffs :: [Int] -> [Int]
diffs (x:xs@(y:ys)) = (y-x) : (diffs xs)
diffs _ = []

-- mains

mainA :: IO ()
mainA = do
    (Just report) <- parseInput reportP "09/input.txt"
    let answer = sum . map prediction $ report
    print answer
    -- result <- submitAnswer 2023 09 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just report) <- parseInput reportP "09/input.txt"
    let answer = sum . map extrapolation $ report
    print answer
    -- result <- submitAnswer 2023 09 2 answer
    -- print result
    return ()
