module Puzzle_2022_05 where

import Automation (submitAnswer)
import Parsing
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List (transpose)

-- data types
type Instruction = (Int, Int, Int)

-- parsing
craneP :: Parser (Vector String, [Instruction])
craneP = do
    grid <- someTill crateLineP indexLineP
    -- index line
    newline
    instructions <- instructionsP
    return (stackCrates grid, instructions)


crateLineP :: Parser String
crateLineP = someTill gridP newline 


gridP :: Parser Char
gridP = do
    g <- (crateP <|> spaceP) 
    optional (char ' ')
    return g


crateP :: Parser Char
crateP = between (char '[') (char ']') upperChar


spaceP :: Parser Char
spaceP = do
    count 3 (char ' ')
    return ' '


indexLineP :: Parser Char
indexLineP = skipManyTill (digitChar <|> char ' ') newline 


instructionsP :: Parser [Instruction]
instructionsP = sepBy instructionP newline


instructionP :: Parser Instruction
instructionP = do
    string "move "
    amount <- natural
    string " from "
    from <- natural
    string " to "
    to <- natural
    return (amount, from - 1, to - 1)


stackCrates :: [String] -> Vector String
stackCrates grid = V.fromList $ map (filter (/= ' ')) transposed 
  where
    transposed = transpose grid


-- functions
rearrange :: Vector String -> [Instruction] -> Vector String
rearrange crates instructions = foldl f crates instructions where
    f :: Vector String -> Instruction -> Vector String
    f cs (amount, from, to) = cs V.// [(from, drop amount s1), (to, move amount s1 s2)]
      where 
        s1 = cs V.! from
        s2 = cs V.! to


rearrange9001 :: Vector String -> [Instruction] -> Vector String
rearrange9001 crates instructions = foldl f crates instructions where
    f :: Vector String -> Instruction -> Vector String
    f cs (amount, from, to) = cs V.// [(from, s1'), (to, s2')]
      where 
        s1 = cs V.! from
        s2 = cs V.! to
        s1' = drop amount s1
        s2' = (take amount s1) ++ s2


move :: Int -> [a] -> [a] -> [a]
move 0 _ to = to
move n (x:from) to = move (n-1) from (x:to)
move _ [] _ = error "Ran out of things to move"


topCrates :: Vector String -> String
topCrates v = V.toList $ V.map head v

-- mains

mainA :: IO ()
mainA = do
    (Just (crates, instructions)) <- parseInput craneP "05/input.txt"
    let answer = topCrates $ rearrange crates instructions
    print answer
    -- result <- submitAnswer 2022 05 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just (crates, instructions)) <- parseInput craneP "05/input.txt"
    let answer = topCrates $ rearrange9001 crates instructions
    print answer
    -- result <- submitAnswer 2022 05 2 answer
    -- print result
    return ()
