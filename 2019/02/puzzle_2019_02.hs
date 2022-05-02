module Puzzle_2019_02 where

import Automation (submitAnswer)
import Intcode
import Parsing (parseInput)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V


gravityAssistSearch :: Program -> Int -> (Int, Int)
gravityAssistSearch prog goal = search [(x, y) | x <- [0..99], y <- [0..99]]
  where
    search [] = (-1, -1)
    search ((n, v):ls)
        | goal == (V.head . execute . (loadProgram (n, v)) $ prog) = (n, v)
        | otherwise = search ls


loadProgram :: (Int, Int) -> Program -> Program
loadProgram (noun, verb) prog = prog V.// [(1, noun), (2, verb)]



mainA :: IO ()
mainA = do
    (Just prog) <- parseInput programP "input.txt"
    let modifiedProg = loadProgram (12, 2) prog
    let answer = (execute modifiedProg) V.! 0
    print answer
    -- result <- submitAnswer 2019 02 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just prog) <- parseInput programP "input.txt"
    let (noun, verb) = gravityAssistSearch prog 19690720
    let answer = 100*noun + verb
    print answer
    -- result <- submitAnswer 2019 02 2 answer
    -- print result
    return ()
