module Puzzle_2022_01 where

import Automation (submitAnswer)
import Parsing


elvesP :: Parser [[Int]]
elvesP = sepBy elfP newline


elfP :: Parser [Int]
elfP = some foodP


foodP :: Parser Int
foodP = do
    food <- natural
    newline
    return food


findMax :: [[Int]] -> Int
findMax elves = maximum $ map sum elves


findTopThree :: [[Int]] -> Int
findTopThree elves = a + b + c where
    (a, b, c) = topThree $ map sum elves


topThree :: [Int] -> (Int, Int, Int)
topThree calories = foldl finder (0, 0, 0) calories where
    finder :: (Int, Int, Int) -> Int -> (Int, Int, Int)
    finder (a, b, c) x
        | x > a     = (x, a, b)
        | x > b     = (a, x, b)
        | x > c     = (a, b, x)
        | otherwise = (a, b, c)


mainA :: IO ()
mainA = do
    (Just elves) <- parseInput elvesP "01/input.txt"
    let answer = findMax elves
    print answer
    -- result <- submitAnswer 2022 01 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just elves) <- parseInput elvesP "01/input.txt"
    let answer = findTopThree elves
    print answer
    -- result <- submitAnswer 2022 01 2 answer
    -- print result
    return ()
