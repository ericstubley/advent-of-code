module Puzzle_2020_01 where

import Automation (submitAnswer)
import Parsing
import Data.Set (Set)
import qualified Data.Set as S

-- parsing
expensesP :: Parser (Set Int)
expensesP = do
    es <- sepBy integer newline
    return $ S.fromList es


-- actual functions
findPair :: Int -> Set Int -> Maybe Int
findPair goal s = S.lookupMax filtered >>= (\x -> Just (x * (goal - x)))
  where filtered = S.filter (\x -> S.member (goal - x) s) s


findMatch :: Set Int -> Int
findMatch s = prod
  where (Just prod) = findPair 2020 s


findThreeMatch :: Set Int -> Int
findThreeMatch s = m * prod
  where m = S.findMax $ S.filter cond s
        cond a = findPair (2020 - a) (S.delete a s) /= Nothing
        (Just prod) = findPair (2020-m) (S.delete m s)



mainA :: IO ()
mainA = do
    (Just expenses) <- parseInput expensesP "01/input.txt"
    let answer = findMatch expenses
    print answer
    -- result <- submitAnswer 2020 01 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just expenses) <- parseInput expensesP "01/input.txt"
    let answer = findThreeMatch expenses
    print answer
    -- result <- submitAnswer 2020 01 2 answer
    -- print result
    return ()
