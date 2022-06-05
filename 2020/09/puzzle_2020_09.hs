module Puzzle_2020_09 where

import Automation (submitAnswer)
import Parsing (parseInput, sepBy, newline, Parser, integer)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq


-- parsing
xmasP :: Parser [Int]
xmasP = sepBy integer newline


-- functions

findError :: Int -> [Int] -> Int
findError n xmas = helper (take n xmas) (drop n xmas)
  where helper :: [Int] -> [Int] -> Int
        helper _ [] = error "Ran out of input"
        helper q (x:xs)
            | prevSumTest q x = helper (tail q ++ [x]) xs
            | otherwise       = x


-- test to see if x is a sum of two distinct elements of q
prevSumTest :: [Int] -> Int -> Bool
prevSumTest q x = any cond q
  where s = S.fromList q
        cond y = S.member (x-y) s && x /= 2*y



encryptionWeakness :: Int -> [Int] -> Int
encryptionWeakness n xmas = minimum range + maximum range
  where target = findError n xmas
        range = search target xmas


-- this doesn't actually enforce the "sum of at least 2" constraint
-- as long as the range we're seeking is before the number itself we're fine
search :: Int -> [Int] -> [Int]
search target xmas = go (sum (take 2 xmas)) (take 2 xmas) (drop 2 xmas)
  where go :: Int -> [Int] -> [Int] -> [Int]
        go s range@(r:range') todo@(t:todo')
            | s <  target = go (s+t) (range ++ [t]) todo'
            | s >  target = go (s-r) range' todo
            | s == target = range



-- mains

mainA :: IO ()
mainA = do
    (Just xmas) <- parseInput xmasP "09/input.txt"
    let answer = findError 25 xmas
    print answer
    -- result <- submitAnswer 2020 09 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just xmas) <- parseInput xmasP "09/input.txt"
    let answer = encryptionWeakness 25 xmas
    print answer
    -- result <- submitAnswer 2020 09 2 answer
    -- print result
    return ()
