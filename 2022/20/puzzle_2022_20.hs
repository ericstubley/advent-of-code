module Puzzle_2022_20 where

import Automation (submitAnswer)
import Parsing
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Sort (sortOn)

-- data types
-- map original index -> (value, currentIndex)
type File = Vector (Int, Int)

-- parsing
fileP :: Parser [Int]
fileP = sepBy integer newline

-- functions
prepare :: [Int] -> File
prepare is = V.fromList $ zip is [0..]


mix :: File -> File
mix file = go file 0 (V.length file)


go :: File -> Int -> Int -> File
go f n limit
    | n == limit = f
    | otherwise  = go f' (n+1) limit
      where 
        m = V.length f
        (jump, old) = f V.! n
        new = mod (old + jump) (m-1)
        mapper (v, i) = (v, mod (shuffle old new i) m)
        f' = V.map mapper f



shuffle :: Int -> Int -> Int -> Int
shuffle old new current
    | old == current                  = new
    | old < current && current < new  = current - 1
    | new < current && current < old  = current + 1
    | old < current && current == new = current - 1
    | current < old && current == new = current + 1
    | otherwise                       = current


reindex :: File -> File
reindex file = V.map adjust file where
    m = V.length file
    zeroIndex = snd . head . dropWhile (\x -> fst x /= 0) $ V.toList file
    adjust = \x -> (fst x, mod (snd x - zeroIndex) m)


find :: File -> Int -> Int
find file i = fst . head . dropWhile (\x -> snd x /= i) $ V.toList file


finalList :: [Int] -> [Int]
finalList is = map fst . sortOn snd . V.toList . reindex . mix . prepare $ is


intermediateList :: [Int] -> Int -> [Int]
intermediateList is n = map fst . sortOn snd . V.toList . (\f -> go f 0 n) . prepare $ is


groveCoordinateSum :: [Int] -> Int
groveCoordinateSum is = (find f i1) + (find f i2) + (find f i3) where
    m = length is
    f = reindex . mix . prepare $ is
    i1 = mod 1000 m
    i2 = mod 2000 m
    i3 = mod 3000 m


decryptionKey :: Int
decryptionKey = 811589153


prePrepare :: [Int] -> [Int]
prePrepare = map ((*) decryptionKey)


finalList' :: [Int] -> Int -> [Int]
finalList' is n = map fst . sortOn snd . V.toList . reindex . mixN n . prepare . prePrepare $ is


mixN :: Int -> File -> File
mixN n f = head . drop n . iterate mix $ f


groveCoordinateSum' :: [Int] -> Int
groveCoordinateSum' is = (find f i1) + (find f i2) + (find f i3) where
    m = length is
    f = reindex . mixN 10 . prepare . prePrepare $ is
    i1 = mod 1000 m
    i2 = mod 2000 m
    i3 = mod 3000 m


-- mains

mainA :: IO ()
mainA = do
    (Just file) <- parseInput fileP "20/input.txt"
    let answer = groveCoordinateSum file
    print answer
    -- result <- submitAnswer 2022 20 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just file) <- parseInput fileP "20/input.txt"
    let answer = groveCoordinateSum' (prePrepare file)
    print answer
    -- result <- submitAnswer 2022 20 2 answer
    -- print result
    return ()
