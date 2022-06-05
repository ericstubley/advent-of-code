module Puzzle_2020_10 where

import Automation (submitAnswer)
import Parsing
import Data.Sort (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad.State

-- type
type Memo = Map (Int, [Int]) Int

-- parsing

adaptersP :: Parser [Int]
adaptersP = sepBy integer newline


-- functions

joltageDiffs :: [Int] -> Int
joltageDiffs adapters = ones*threes
  where chain = (0 : sort adapters) ++ [3 + maximum adapters]
        (ones, _, threes) = countDiffs chain


countDiffs :: [Int] -> (Int, Int, Int)
countDiffs chain = go chain (0, 0, 0)
  where go :: [Int] -> (Int, Int, Int) -> (Int, Int, Int)
        go [] (a, b, c) = (a, b, c)
        go (x:[]) (a, b, c) = (a, b, c)
        go (x:xs@(y:_)) (a, b, c) = 
            case (y-x) of
                1 -> go xs (a+1, b, c)
                2 -> go xs (a, b+1, c)
                3 -> go xs (a, b, c+1)


countChains :: [Int] -> Int
countChains adapters = evalState (counter 0 (sort adapters)) M.empty


counter :: MonadState Memo m => Int -> [Int] -> m Int
counter jolts (x:[])
    | x - jolts <= 3 = return 1
    | otherwise      = return 0
counter jolts (x:xs) = do
    cached <- gets (M.lookup (jolts, x:xs))
    case cached of
        (Just c) -> return c
        Nothing  -> do
            if x-jolts > 3
                then return 0
                else do
                    a <- counter jolts xs
                    b <- counter x xs
                    modify $ M.insert (jolts, x:xs) (a+b)
                    return (a+b)

-- mains

mainA :: IO ()
mainA = do
    (Just adapters) <- parseInput adaptersP "10/input.txt"
    let answer = joltageDiffs adapters
    print answer
    -- result <- submitAnswer 2020 10 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just adapters) <- parseInput adaptersP "10/input.txt"
    let answer = countChains adapters
    print answer
    -- result <- submitAnswer 2020 10 2 answer
    -- print result
    return ()
