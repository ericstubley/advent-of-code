module Puzzle_2021_01 where

import Automation (submitAnswer)
import Parsing

-- data types

-- parsing
depthsP :: Parser [Int]
depthsP = sepBy integer newline

-- functions
increases :: [Int] -> Int
increases ds = length . filter (\t -> fst t < snd t) $ zip ds (tail ds)


window3 :: [Int] -> [Int]
window3 ds = zipWith3 (\a b c -> a + b + c) ds (tail ds) (drop 2 ds)


increases' :: [Int] -> Int
increases' = increases . window3

-- mains

mainA :: IO ()
mainA = do
    (Just depths) <- parseInput depthsP "01/input.txt"
    let answer = increases depths
    print answer
    -- result <- submitAnswer 2021 01 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just depths) <- parseInput depthsP "01/input.txt"
    let answer = increases' depths
    print answer
    -- result <- submitAnswer 2021 01 2 answer
    -- print result
    return ()
