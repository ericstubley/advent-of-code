module Puzzle_2019_16 where

import Automation (submitAnswer)
import Parsing
import Data.List (foldl', scanl', foldl1')

-- parsing
digitsP :: Parser [Int]
digitsP = do
    cs <- many digitChar
    return $ map (\c -> read [c]) cs


-- functions
-- take a positive integral input and convert to a list of digits
toDigits :: Int -> [Int]
toDigits n = go n [] where
    go 0 ds = ds
    go n ds = go (div n 10) ((mod n 10) : ds)


-- convert a list of digits to an Int; note that this will behave
-- in weird ways for long lists
fromDigits :: [Int] -> Int
fromDigits = foldl' (\acc x -> 10*acc + x) 0


transformed :: [Int] -> [Int] -> Int
transformed xs patt = (`mod` 10) . abs . sum . (zipWith (*) xs) $ patt


fft :: [Int] -> [Int]
fft xs = map ((transformed xs) . pattern) [1..(length xs)]


iterativeFFT :: Int -> [Int] -> [Int]
iterativeFFT n xs = (iterate fft xs) !! n


base :: [Int]
base = [0, 1, 0, -1]


pattern :: Int -> [Int]
pattern n = drop 1 . cycle . expand n $ base


expand :: Int -> [Int] -> [Int]
expand n [] = []
expand n (x:xs) = (take n $ repeat x) ++ expand n xs


offset :: [Int] -> Int
offset = fromDigits . (take 7)


decode :: [Int] -> Int
decode xs = fromDigits $ take 8 $ reverse $ (iterate summer lxs) !! 100
  where
    o = offset xs
    lxs = reverse $ drop o $ take (10000*(length xs)) (cycle xs)
    summer = scanl' (\x y -> mod (x+y) 10) 0


-- compute the n-th digit of the input list * 10000
longInput :: [Int] -> Int -> Int
longInput xs n = xs !! (mod n 10000)


-- mains

mainA :: IO ()
mainA = do
    (Just digits) <- parseInput digitsP "16/input.txt"
    let answer = fromDigits $ take 8 $ iterativeFFT 100 digits
    print answer
    -- result <- submitAnswer 2019 16 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just digits) <- parseInput digitsP "16/input.txt"
    let answer = decode digits
    print answer
    -- result <- submitAnswer 2019 16 2 answer
    -- print result
    return ()
