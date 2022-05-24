module Puzzle_2019_02 where

import Automation (submitAnswer)
import Intcode
import Parsing (parseInput)


gravityAssistSearch :: Program -> Int -> (Int, Int)
gravityAssistSearch prog goal = search [(x, y) | x <- [0..99], y <- [0..99]]
  where search :: [(Int, Int)] -> (Int, Int)
        search [] = (-1, -1)
        search ((n, v):ls) 
            | head beheaded == goal = (n, v)
            | otherwise             = search ls
            where (beheaded, _) = runProgram (loadProgram (n, v) prog) []


loadProgram :: (Int, Int) -> Program -> Program
loadProgram (noun, verb) prog = head prog : noun : verb : drop 3 prog



mainA :: IO ()
mainA = do
    (Just prog) <- parseInput programP "02/input.txt"
    let modifiedProg = loadProgram (12, 2) prog
    let (finalProg, _) = runProgram modifiedProg []
    let answer = head finalProg
    print answer
    -- result <- submitAnswer 2019 02 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just prog) <- parseInput programP "02/input.txt"
    let (noun, verb) = gravityAssistSearch prog 19690720
    let answer = 100*noun + verb
    print answer
    -- result <- submitAnswer 2019 02 2 answer
    -- print result
    return ()
