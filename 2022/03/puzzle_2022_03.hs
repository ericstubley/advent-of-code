module Puzzle_2022_03 where

import Automation (submitAnswer)
import Parsing
import Data.Char (ord)
import Data.Set (Set)
import qualified Data.Set as S

-- data types

-- parsing
bagsP :: Parser [String]
bagsP = sepBy bagP newline


bagP :: Parser String
bagP = some letterChar

-- functions
-- given a string of length 2n with exactly one character in common between
-- the first n and the last n, return that common character
overlap :: String -> Char
overlap bag = search back
  where
    n = length bag
    half = div n 2
    front = S.fromList $ take half bag
    back = drop half bag
    search :: String -> Char
    search [] = error "No match found"
    search (x:xs)
        | S.member x front = x
        | otherwise        = search xs

priority :: Char -> Int
priority c
    | c >= 'a'  = (ord c) - 96
    | otherwise = (ord c) - 64 + 26


prioritySum :: [String] -> Int
prioritySum bags = sum $ map (priority . overlap) bags


badges :: [String] -> String
badges [] = []
badges (x:y:z:xs) = badge : badges xs
  where sx = S.fromList x
        sy = S.fromList y
        sz = S.fromList z
        badge = S.findMin $ S.intersection (S.intersection sx sy) sz
badges _ = error "Wrong number of bags mod 3"


badgeSum :: [String] -> Int
badgeSum bags = sum $ map priority (badges bags)

-- mains

mainA :: IO ()
mainA = do
    (Just bags) <- parseInput bagsP "03/input.txt"
    let answer = prioritySum bags
    print answer
    -- result <- submitAnswer 2022 03 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just bags) <- parseInput bagsP "03/input.txt"
    let answer = badgeSum bags
    print answer
    -- result <- submitAnswer 2022 03 2 answer
    -- print result
    return ()
