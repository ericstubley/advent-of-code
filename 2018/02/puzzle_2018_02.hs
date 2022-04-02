module Puzzle_2018_02 where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.IO
import Automation (submitAnswer)

parseInput :: String -> IO [String]
parseInput filename = do
    raw <- readFile filename
    return $ lines raw


checksum :: [String] -> Int
checksum ids = doubles * triples where
    doubles = length $ filter hasDoubleLetter ids
    triples = length $ filter hasTripleLetter ids


counts :: Ord a => [a] -> Map a Int
counts = foldl (\acc x -> Map.insertWith (+) x 1 acc) Map.empty


hasNRepeat :: Ord a => Int -> [a] -> Bool
hasNRepeat n ids = Map.empty /= Map.filter (n==) (counts ids)


hasDoubleLetter = hasNRepeat 2
hasTripleLetter = hasNRepeat 3


findCorrect :: Eq a => [[a]] -> [a]
findCorrect (x:xs)
    | length (matches x xs) > 0 = extractLetters x (head $ matches x xs)
    | otherwise                 = findCorrect xs
        where matches x xs = filter (offByOne x) xs


offByOne :: Eq a => [a] -> [a] -> Bool
offByOne [] [] = False
offByOne (x:xs) (y:ys)
    | x == y = offByOne xs ys
    | x /= y = (xs == ys)


extractLetters :: Eq a => [a] -> [a] -> [a]
extractLetters xs ys = foldl extract [] (zip xs ys) where
    extract acc (x, y)
        | x == y    = acc ++ [x]
        | otherwise = acc


mainA :: IO ()
mainA = do
    ids <- parseInput "input.txt"
    let answer = checksum ids
    print answer
    -- result <- submitAnswer 2018 02 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    ids <- parseInput "input.txt"
    let answer = findCorrect ids
    print answer
    -- result <- submitAnswer 2018 02 2 answer
    -- print result
    return ()
