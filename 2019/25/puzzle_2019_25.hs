module Puzzle_2019_25 where

import Automation (submitAnswer)
import Intcode
import Parsing


weightWatcher :: String
weightWatcher = unlines 
    [ "west"
    , "west"
    , "take bowl of rice"
    , "east"
    , "north"
    , "east"
    , "south"
    , "take dark matter"
    , "north"
    , "west"
    , "north"
    , "take candy cane"
    , "west"
    , "west"
    , "north"
    , "take dehydrated water"
    , "west"
    , "south"]


mainA :: IO ()
mainA = do
    (Just rpg) <- parseInput programP "25/input.txt"
    -- runInteractiveAscii rpg
    let (_, output) = runProgramAscii rpg weightWatcher 
    let answer = last $ lines output
    print answer
    -- result <- submitAnswer 2019 25 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    let answer = 0
    print answer
    -- result <- submitAnswer 2019 25 2 answer
    -- print result
    return ()
