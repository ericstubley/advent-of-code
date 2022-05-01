module Puzzle_2019_01 where

import System.IO
import Automation (submitAnswer)
import Parsing


massP :: Parser [Int]
massP = sepBy natural newline


fuel :: Int -> Int
fuel mass = (div mass 3) - 2


fuel' :: Int -> Int
fuel' mass
    | mass <= 6 = 0
    | otherwise = mf + fuel' mf
    where mf = fuel mass


mainA :: IO ()
mainA = do
    (Just masses) <- parseInput "input.txt" massP
    let answer = sum $ map fuel masses
    print answer
    -- result <- submitAnswer 2019 01 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just masses) <- parseInput "input.txt" massP
    let answer = sum $ map fuel' masses
    print answer
    -- result <- submitAnswer 2019 01 2 answer
    -- print result
    return ()
