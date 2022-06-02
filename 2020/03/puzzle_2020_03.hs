module Puzzle_2020_03 where

import Automation (submitAnswer)
import Parsing
import Grid
import qualified Data.Massiv.Array as A

-- parsing
treesP :: Parser (Array2 Char)
treesP = do
    trees <- sepBy rowP newline
    return $ A.fromLists' A.Seq trees


rowP :: Parser [Char]
rowP = some (char '.' <|> char '#')


-- functions
treesHit :: Ix2 -> Array2 Char -> Int
treesHit (si:.sj) trees = length $ filter (== '#') path
  where (A.Sz (li:.lj)) = A.size trees
        inBounds (i:.j) = 0 <= i && i < li
        ixs = takeWhile inBounds [n*si :. (mod (n*sj) lj) | n <- [0..]]
        path = map (\ix -> A.index' trees ix) ixs


averageHits :: Array2 Char -> Int
averageHits trees = product $ map (\s -> treesHit s trees) slopes
  where slopes = [1:.1, 1:.3, 1:.5, 1:.7, 2:.1]

-- mains

mainA :: IO ()
mainA = do
    (Just trees) <- parseInput treesP "03/input.txt"
    let answer = treesHit (1:.3) trees
    print answer
    -- result <- submitAnswer 2020 03 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just trees) <- parseInput treesP "03/input.txt"
    let answer = averageHits trees
    print answer
    -- result <- submitAnswer 2020 03 2 answer
    -- print result
    return ()
