module Puzzle_2019_09 where

import Automation (submitAnswer)
import Parsing
import Intcode

mainA :: IO ()
mainA = do
    (Just p) <- parseInput programP "09/input.txt"
    let (p', out) = runProgram p [1]
    let answer = head out
    print answer
    -- result <- submitAnswer 2019 09 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just p) <- parseInput programP "09/input.txt"
    let (p', out) = runProgram p [2]
    print out
    let answer = head out
    -- print answer
    result <- submitAnswer 2019 09 2 answer
    print result
    return ()
