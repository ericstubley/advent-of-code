module Puzzle_2022_10 where

import Automation (submitAnswer)
import Parsing
import Data.List.Split (chunksOf)

-- data types

data Instruction = NOOP | ADDX Int deriving (Eq, Show, Ord)


-- parsing

programP :: Parser [Instruction]
programP = sepBy instructionP newline


instructionP :: Parser Instruction
instructionP = noopP <|> addxP


noopP :: Parser Instruction
noopP = string "noop" >> return NOOP


addxP :: Parser Instruction
addxP = do
    string "addx "
    n <- integer
    return $ ADDX n


-- functions
register :: [Instruction] -> [Int]
register = register' 1


register' :: Int -> [Instruction] -> [Int]
register' _ [] = []
register' x (i:is) = case i of
                        NOOP   -> x : register' x is
                        ADDX n -> x : x : register' (x+n) is


signalStrengths :: [Instruction] -> [Int]
signalStrengths = zipWith (*) [1..] . register


takeEvery :: Int -> [a] -> [a]
takeEvery _ [] = []
takeEvery n xs = case drop (n-1) xs of
                        []     -> []
                        (y:ys) -> y : takeEvery n ys


-- every a mod m-th element of the list
-- should only use with a < m
arithmeticProgression :: Int -> Int -> [a] -> [a]
arithmeticProgression _ _ [] = []
arithmeticProgression a m xs = case drop (a-1) xs of
                                    []     -> []
                                    (y:ys) -> y : takeEvery m ys


sumInterestingSignalStrengths :: [Instruction] -> Int
sumInterestingSignalStrengths = sum . arithmeticProgression 20 40 . signalStrengths


lit :: Bool -> Char
lit True = '#'
lit False = '.'


isLit :: Int -> Int -> Bool
isLit electron sprite = abs ((mod electron 40) - sprite) <= 1


screen :: [Instruction] -> [String]
screen = chunksOf 40 . map lit . zipWith isLit [0..239] . register


-- mains

mainA :: IO ()
mainA = do
    (Just program) <- parseInput programP "10/input.txt"
    let answer = sumInterestingSignalStrengths program
    print answer
    -- result <- submitAnswer 2022 10 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just program) <- parseInput programP "10/input.txt"
    mapM_ putStrLn . screen $ program
    -- let answer = 0
    -- print answer
    -- result <- submitAnswer 2022 10 2 answer
    -- print result
    return ()
