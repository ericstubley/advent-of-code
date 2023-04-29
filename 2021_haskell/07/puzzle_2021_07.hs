module Puzzle_2021_07 where

import Automation (submitAnswer)
import Parsing
import Data.Sort (sort)
import Data.Ratio ((%))

-- data types

-- parsing
crabsP :: Parser [Int]
crabsP = sepBy integer (char ',')

-- functions
linearMinimize :: [Int] -> Int
linearMinimize crabs = sum $ map (linearCostFrom median) crabs
  where median = (sort crabs) !! (div (length crabs) 2)


linearCostFrom :: Int -> Int -> Int
linearCostFrom x y = abs (x - y)


quadraticMinimize :: [Int] -> Int
quadraticMinimize crabs = min down up
  where mean_floor = floor ((sum crabs) % (length crabs))
        mean_ceiling = ceiling ((sum crabs) % (length crabs))
        down = sum $ map (quadraticCostFrom mean_floor) crabs
        up = sum $ map (quadraticCostFrom mean_ceiling) crabs


quadraticCostFrom :: Int -> Int -> Int
quadraticCostFrom x y = div (d * (d + 1)) 2
  where d = abs (x - y)


-- mains

mainA :: IO ()
mainA = do
    (Just crabs) <- parseInput crabsP "07/input.txt"
    let answer = linearMinimize crabs
    print answer
    -- result <- submitAnswer 2021 07 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just crabs) <- parseInput crabsP "07/input.txt"
    let answer = quadraticMinimize crabs
    print answer
    -- result <- submitAnswer 2021 07 2 answer
    -- print result
    return ()
