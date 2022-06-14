module Puzzle_2020_25 where

import Automation (submitAnswer)
import Data.Bits


-- mod 20201227
base :: Int
base = 20201227
-- generator 7
discreteLog :: Int -> Int
discreteLog n = go 0 1
  where go :: Int -> Int -> Int
        go e k
            | k == n    = e
            | otherwise = go (e+1) (mod (7*k) base)


encryptionKey :: Int -> Int -> Int
encryptionKey p1 p2 = modExp p2 (discreteLog p1) base


modExp :: Int -> Int -> Int -> Int
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
           where t = if testBit e 0 then b `mod` m else 1

mainA :: IO ()
mainA = do
    let answer = encryptionKey 19241437 17346587
    print answer
    result <- submitAnswer 2020 25 1 answer
    print result
    return ()


mainB :: IO ()
mainB = do
    let answer = 0
    print answer
    -- result <- submitAnswer 2020 25 2 answer
    -- print result
    return ()
