module Puzzle_2022_09 where

import Automation (submitAnswer)
import Parsing
import Data.List (nub)
import Grid (Direction(..), dirVec)

-- data types
data Instruction = Instruction Direction Int
    deriving (Eq, Show, Ord)

type Coord = (Int, Int)

-- parsing
instructionsP :: Parser [Instruction]
instructionsP = sepBy instructionP newline


instructionP :: Parser Instruction
instructionP = do
    dir <- directionP
    space
    dist <- natural
    return $ Instruction dir dist


directionP :: Parser Direction
directionP =   (char 'R' >> return East) 
           <|> (char 'L' >> return West)
           <|> (char 'U' >> return North)
           <|> (char 'D' >> return South)


-- functions
headTrack :: [Instruction] -> [Coord]
headTrack = headTrack' (0, 0)


headTrack' :: Coord -> [Instruction] -> [Coord]
headTrack' pos [] = [pos]
headTrack' pos (i:is)
    | dist > 0  = pos : headTrack' pos' (i':is)
    | otherwise = headTrack' pos is
      where
        (x, y) = pos
        Instruction dir dist = i
        (dx, dy) = dirVec dir
        pos' = (x+dx, y+dy)
        i' = Instruction dir (dist-1)


tailTrack :: [Instruction] -> [Coord]
tailTrack instructions = track (headTrack instructions)


track :: [Coord] -> [Coord]
track = tail . (scanl update (0, 0))


clip :: Int -> Int
clip x
    | x == 0 = 0
    | x > 0  = 1
    | x < 0  = -1


-- movement condition is one difference is at least 2, not manhattan >= 2
update :: Coord -> Coord -> Coord
update (tx, ty) (hx, hy)
    | max (abs dx) (abs dy) >= 2 = (tx + clip dx, ty + clip dy)
    | otherwise                  = (tx, ty)
        where (dx, dy) = (hx - tx, hy - ty)


countTailPositions :: [Instruction] -> Int
countTailPositions = countLongTailPositions 2


-- pretend the rope is infinite! generate a list of the tracks of the knots
-- then just look at the number of positions in the nth one
countLongTailPositions :: Int -> [Instruction] -> Int
countLongTailPositions n = length . nub . last . (take n) . (iterate track) . headTrack


-- mains

mainA :: IO ()
mainA = do
    (Just instructions) <- parseInput instructionsP "09/input.txt"
    let answer = countTailPositions instructions
    print answer
    -- result <- submitAnswer 2022 09 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just instructions) <- parseInput instructionsP "09/input.txt"
    let answer = countLongTailPositions 10 instructions
    print answer
    -- result <- submitAnswer 2022 09 2 answer
    -- print result
    return ()
