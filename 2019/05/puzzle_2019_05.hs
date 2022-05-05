module Puzzle_2019_05 where

import Automation (submitAnswer)
import Intcode
import Parsing (parseInput)

mainA :: IO ()
mainA = do
    (Just prog) <- parseInput programP "05/input.txt"
    -- should have a single input of 1, then output a bunch of 0s, 
    -- then output the answer and exit
    execute prog
    -- let answer = 0 
    -- print answer
    -- result <- submitAnswer 2019 05 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just prog) <- parseInput programP "05/input.txt"
    -- should have a single input of 5, then output the answer and exit
    execute prog
    -- let answer = 0
    -- print answer
    -- result <- submitAnswer 2019 05 2 answer
    -- print result
    return ()
