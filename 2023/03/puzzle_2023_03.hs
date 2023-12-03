module Puzzle_2023_03 where

import Automation (submitAnswer)
import Parsing
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Massiv.Core (Ix2(..))
import Data.Char(isDigit)
import Data.List (tails)


import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- data types

-- parsing
inputP :: Parser ([(Ix2, Int)], Map Ix2 Char)
inputP = do
    rows <- sepBy rowP newline
    let numbers = builder extractNumbers rows
    let symbols = Map.fromList $ builder extractSymbols rows
    return (numbers, symbols)

rowP :: Parser String
rowP = many printChar

-- functions
-- given an input row, construct a list of (i, char) for the symbols
extractSymbols :: String -> [(Int, Char)]
extractSymbols row = ifoldl appender [] row
  where appender acc i c = case isSymbol c of
                            True  -> (i, c) : acc
                            False -> acc

-- given an input row, construct a list of (i, n) for the numbers
extractNumbers :: String -> [(Int, Int)]
extractNumbers row = extractNumbers' 0 row
  where extractNumbers' i [] = []
        extractNumbers' i xs@(y:ys)
            | isDigit y = (i, number) : extractNumbers' (i+l) remainder
            | otherwise = extractNumbers' (i+1) ys
              where (extracted, remainder) = span isDigit xs
                    l = length extracted
                    number = (read extracted) :: Int


builder :: (String -> [(Int, a)]) -> [String] -> [(Ix2, a)]
builder f input = concat $ imap (\i xs -> map (coordinator i) xs) (map f input)
  where coordinator i (j, x) = (i :. j, x)

partNumbers :: [(Ix2, Int)] -> Map Ix2 Char -> [Int]
partNumbers numbers symbols = map snd $ filter (isPartNumber symbols) numbers

isPartNumber :: Map Ix2 Char -> (Ix2, Int) -> Bool
isPartNumber symbols number = any (\x -> Map.member x symbols) (boundary number)

-- part b

gearRatioSum :: [(Ix2, Int)] -> Map Ix2 Char -> Int
gearRatioSum numbers symbols = sum gearRatios
  where symbols' = Map.toList symbols
        boundaries = map (\x -> constantMap (boundary x) [snd x]) numbers 
        numberMap = Map.unionsWith (++) boundaries
        gears = filter (isGear numberMap) symbols'
        gearRatios = map (\x -> product (numberMap Map.! (fst x))) gears

isGear :: Map Ix2 [Int] -> (Ix2, Char) -> Bool
isGear numberMap (ix, c) = c == '*' 
                         && Map.member ix numberMap 
                         && length (numberMap Map.! ix) == 2

-- generate the map of (boundary coord -> list of numbers having that as a bounday coord)
-- for each * symbol
--      



-- helpers
isSymbol :: Char -> Bool
isSymbol x = not (isDigit x) && not (x == '.')

numDigits :: Int -> Int
numDigits = length . show

boundary :: (Ix2, Int) -> [Ix2]
boundary (i:.j, n) = top ++ bottom ++ [left] ++ [right]
  where l = numDigits n
        top = [(i-1) :. y | y <- [(j-1)..(j+l)]]
        bottom = [(i+1) :. y | y <- [(j-1)..(j+l)]]
        left = i :. (j-1)
        right = i :. (j+l)

-- could actually be Foldable instead of [.] but whatever
ifoldl :: (b -> Int -> a -> b) -> b -> [a] -> b
ifoldl f acc xs = foldl (\x y -> f x (fst y) (snd y)) acc (zip [0..] xs)

imap :: (Int -> a -> b) -> [a] -> [b]
imap f xs = map (\x -> f (fst x) (snd x)) (zip [0..] xs)


constantMap :: Ord a => [a] -> b -> Map a b
constantMap ixs c = Map.fromList $ zip ixs (repeat c)


-- mains

mainA :: IO ()
mainA = do
    (Just (numbers, symbols)) <- parseInput inputP "03/input.txt"
    let answer = sum (partNumbers numbers symbols)
    print answer
    -- result <- submitAnswer 2023 03 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just (numbers, symbols)) <- parseInput inputP "03/input.txt"
    let answer = gearRatioSum numbers symbols
    print answer
    result <- submitAnswer 2023 03 2 answer
    print result
    return ()
