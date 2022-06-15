module Puzzle_2020_23 where

import Automation (submitAnswer)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

-- data types
type Cups = IntMap Int


-- functions
digits :: Int -> [Int]
digits 0 = []
digits n = digits (div n 10) ++ [mod n 10]


layout :: Int -> Cups
layout n = M.fromList $ zip ds (drop 1 $ cycle ds)
  where ds = digits n


layoutBig :: Int -> Cups
layoutBig n = M.fromList $ zip is (drop 1 $ cycle is)
  where is = digits n ++ [10..1000000]


play :: Int -> Int -> Cups
play start rounds = simulate rounds first cups
  where first = head $ digits start
        cups = layout start


playBig :: Int -> Int -> Cups
playBig start rounds = simulate rounds first cups
  where first = head $ digits start
        cups = layoutBig start


simulate :: Int -> Int -> Cups -> Cups
simulate 0 curr cups = cups
simulate rounds curr cups = simulate (rounds-1) curr' cups'
  where cups' = shuffle curr cups
        curr' = cups' M.! curr


after1 :: Cups -> Int
after1 cups = go 0 (cups M.! 1)
  where go :: Int -> Int -> Int
        go acc 1 = acc
        go acc curr = go (10*acc + curr) (cups M.! curr)


nbhd :: Cups -> Int
nbhd cups = first * second
  where first = cups M.! 1
        second = cups M.! first


-- updates to make
-- curr -> (whatever third maps to)
-- dest -> first
-- third -> (whatever dest maps to)
shuffle :: Int -> Cups -> Cups
shuffle curr cups = cups'''
  where first = cups M.! curr
        second = cups M.! first
        third = cups M.! second
        afterThird = cups M.! third
        dest = destination curr cups
        afterDest = cups M.! dest
        cups' = M.insert curr afterThird cups
        cups'' = M.insert dest first cups'
        cups''' = M.insert third afterDest cups''


destination :: Int -> Cups -> Int
destination curr cups = head $ dropWhile (\x -> elem x [first, second, third]) options
  where options = map (center cups) [curr-1, curr-2, curr-3, curr-4]
        first = cups M.! curr
        second = cups M.! first
        third = cups M.! second


center :: Cups -> Int -> Int
center cups n
    | n > 0     = n
    | otherwise = n + (M.size cups)



-- mains

mainA :: IO ()
mainA = do
    let answer = after1 $ play 137826495 100
    print answer
    -- result <- submitAnswer 2020 23 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    let answer = nbhd $ playBig 137826495 10000000
    print answer
    -- result <- submitAnswer 2020 23 2 answer
    -- print result
    return ()
