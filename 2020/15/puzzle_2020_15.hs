module Puzzle_2020_15 where

import Automation (submitAnswer)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

-- functions
-- want to generate an infinite list of the game
-- to generate the next all you need is 
--      a) the record of the last time each number was said
--      b) the last number said
--      c) the current turn number
next :: IntMap Int -> Int -> Int -> Int
next record said turn
    | IM.member said record = turn - (record IM.! said)
    | otherwise             = 0


game :: [Int] -> Int -> Int
game initial desired = go record (last initial) (length initial)
  where record = IM.fromList $ zip (init initial) [1..]
        go :: IntMap Int -> Int -> Int -> Int
        go record said turn 
            | turn == desired = said
            | otherwise       = go record' said' (turn+1)
              where said' = next record said turn
                    record' = IM.insert said turn record

-- mains

mainA :: IO ()
mainA = do
    let initial = [7,14,0,17,11,1,2] 
    let answer = game initial 2020
    print answer
    -- result <- submitAnswer 2020 15 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    let initial = [7,14,0,17,11,1,2] 
    let answer = game initial 30000000
    print answer
    -- result <- submitAnswer 2020 15 2 answer
    -- print result
    return ()
