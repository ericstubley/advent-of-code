module Puzzle_2022_06 where

import Automation (submitAnswer)
import Parsing
import Data.Sequence (Seq(..), ViewL(..))
import Data.Set (Set)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

-- data types

-- parsing

datastreamP :: Parser String
datastreamP = some lowerChar

-- functions
-- doesn't do any input validation! if you don't have a length l distinct run
-- no guarantees that this will behave sensibly
-- returns the index after the distinct run
firstDistinctRun :: (Eq a, Ord a) => [a] -> Int -> Int
firstDistinctRun xs l = fdr (Seq.empty) (Set.empty) xs 0 where
    fdr :: (Eq a, Ord a) => Seq a -> Set a -> [a] -> Int -> Int
    fdr run count xs idx
        | Seq.length run == l               = idx
        | not (Set.member (head xs) count)  = fdr (run Seq.|> x) (Set.insert x count) (tail xs) (idx+1)
        | otherwise                         = fdr left (Set.delete first count) xs idx
          where 
            x = head xs
            (first :< left) = Seq.viewl run



packetStart :: String -> Int
packetStart s = firstDistinctRun s 4


messageStart :: String -> Int
messageStart s = firstDistinctRun s 14

-- mains

mainA :: IO ()
mainA = do
    (Just datastream) <- parseInput datastreamP "06/input.txt"
    let answer = packetStart datastream
    print answer
    -- result <- submitAnswer 2022 06 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just datastream) <- parseInput datastreamP "06/input.txt"
    let answer = messageStart datastream
    print answer
    -- result <- submitAnswer 2022 06 2 answer
    -- print result
    return ()
