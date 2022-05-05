module Puzzle_2019_02 where

import Automation (submitAnswer)
import Intcode
import Parsing (parseInput)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V


gravityAssistSearch :: Program -> Int -> IO (Int, Int)
gravityAssistSearch prog goal = search [(x, y) | x <- [0..99], y <- [0..99]]
  where
    search :: [(Int, Int)] -> IO (Int, Int)
    search [] = return (-1, -1)
    search ((n, v):ls) = do
        beheaded <- execute $ loadProgram (n, v) prog
        if goal == V.head beheaded
            then return (n, v)
            else search ls


loadProgram :: (Int, Int) -> Program -> Program
loadProgram (noun, verb) prog = prog V.// [(1, noun), (2, verb)]



mainA :: IO ()
mainA = do
    (Just prog) <- parseInput programP "02/input.txt"
    let modifiedProg = loadProgram (12, 2) prog
    finalProg <- execute modifiedProg
    let answer = finalProg V.! 0
    print answer
    -- result <- submitAnswer 2019 02 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just prog) <- parseInput programP "02/input.txt"
    (noun, verb) <- gravityAssistSearch prog 19690720
    let answer = 100*noun + verb
    print answer
    -- result <- submitAnswer 2019 02 2 answer
    -- print result
    return ()
