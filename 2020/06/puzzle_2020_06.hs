module Puzzle_2020_06 where

import Automation (submitAnswer)
import Parsing
import Data.Set (Set)
import qualified Data.Set as S

-- parsing
groupsP :: Parser [[String]]
groupsP = sepBy groupP newline

groupP :: Parser [String]
groupP = some personP

personP :: Parser String
personP = some lowerChar <* optional newline


-- functions
score :: [String] -> Int
score group = S.size $ S.fromList (concat group)


score' :: [String] -> Int
score' group = S.size $ foldl1 S.intersection (map S.fromList group)

-- mains

mainA :: IO ()
mainA = do
    (Just groups) <- parseInput groupsP "06/input.txt"
    let answer = sum $ map score groups
    print answer
    -- result <- submitAnswer 2020 06 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just groups) <- parseInput groupsP "06/input.txt"
    let answer = sum $ map score' groups
    print answer
    -- result <- submitAnswer 2020 06 2 answer
    -- print result
    return ()
