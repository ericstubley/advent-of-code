module Puzzle_2021_06 where

import Automation (submitAnswer)
import Parsing
import Data.Vector (Vector)
import Linear.Vector
import qualified Data.Vector as V

-- data types
type Fish = Vector Int

-- parsing
lanternfishP :: Parser Fish
lanternfishP = foldl1 (^+^) <$> sepBy fishP (char ',')


fishP :: Parser Fish
fishP = bucket <$> integer 

-- functions

days :: Int -> Fish -> Fish
days 0 (!fish) = fish
days n (!fish) = days (n-1) (day fish)


day :: Fish -> Fish
day (!fish) = shifted ^+^ spawned
  where shifted = (V.tail fish) V.++ (V.singleton 0)
        spawned = spawn (V.head fish)


spawn :: Int -> Fish
spawn n = n *^ respawn


bucket :: Int -> Fish
bucket i = (V.replicate size 0) V.// [(i, 1)]
  where size = 9


respawn :: Fish  
respawn = (bucket 6) ^+^ (bucket 8)


-- mains

mainA :: IO ()
mainA = do
    (Just lanternfish) <- parseInput lanternfishP "06/input.txt"
    let answer = V.sum $ days 80 lanternfish
    print answer
    -- result <- submitAnswer 2021 06 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just lanternfish) <- parseInput lanternfishP "06/input.txt"
    let answer = V.sum $ days 256 lanternfish
    print answer
    -- result <- submitAnswer 2021 06 2 answer
    -- print result
    return ()
