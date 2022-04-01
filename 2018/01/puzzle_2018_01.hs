module Puzzle_2018_01 where

import System.IO
import Data.Set (Set)
import qualified Data.Set as Set
import Automation (submitAnswer)


parseInput :: String -> IO [Int]
parseInput filename = do
    raw <- readFile filename
    let freqs = map toInt $ lines raw where
        toInt :: String -> Int
        toInt ('+':s) = toInt s
        toInt ('-':s) = -1 * (toInt s)
        toInt s = read s
    return freqs


freqSum :: [Int] -> Int
freqSum = sum


firstRepeat :: [Int] -> Int
firstRepeat fs = helper 0 Set.empty (cycle fs) where
    helper freq set (f:fs)
        | Set.member freq set = freq
        | otherwise           = helper (freq + f) (Set.insert freq set) fs


mainA :: IO ()
mainA = do
    freqs <- parseInput "input.txt"
    let answer = freqSum freqs
    print answer
    -- result <- submitAnswer 2018 01 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    freqs <- parseInput "input.txt"
    let answer = firstRepeat freqs
    print answer
    -- result <- submitAnswer 2018 01 2 answer
    -- print result
    return ()
