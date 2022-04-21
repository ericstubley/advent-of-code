module Puzzle_2018_21 where

import qualified Data.Set as S
import Data.Bits ((.&.), (.|.))
import System.IO
import Automation (submitAnswer)


start :: Int
start = 10649702

mult :: Int
mult = 65899

cutoff :: Int
cutoff = 16777215

lastDigits :: Int -> Int
lastDigits n = n .&. 255


nextSeed :: Int -> Int
nextSeed reg = reg .|. 65536

rng :: Int -> Int
rng seed = foldl (\acc a -> ((acc + a)*mult) .&. cutoff) start digits
  where digits = map lastDigits [seed, div seed 256, div seed 65536]


exits :: [Int]
exits = tail $ iterate (rng . nextSeed) 0


untilDuplicate :: [Int]
untilDuplicate = map fst $ takeWhile (\x -> not $ S.member (fst x) (snd x)) pairs
 where
    sets = scanl (\acc a -> S.insert a acc) S.empty exits
    pairs = zip exits sets


mainA :: IO ()
mainA = do
    let answer = head exits
    print answer
    -- result <- submitAnswer 2018 21 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    let answer = last untilDuplicate
    let confirm = elem (rng . nextSeed $ answer) untilDuplicate
    print confirm
    print answer
    -- result <- submitAnswer 2018 21 2 answer
    -- print result
    return ()
