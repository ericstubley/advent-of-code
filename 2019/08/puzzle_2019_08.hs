module Puzzle_2019_08 where

import Automation (submitAnswer)
import Parsing
import Data.List (foldl1')


-- if you were less lazy you'd convert to a Pixel type instead of Chars
-- but this is a quick and dirty solution!

minimumWith :: Ord b => (a -> b) -> [a] -> a
minimumWith f xs = foldl1' chooser xs where
    chooser x y -- assuming x is the acc argument?
        | f(x) > f(y) = y
        | otherwise   = x


amount :: Eq a => a -> [a] -> Int
amount x xs = length $ filter (==x) xs


layers :: Int -> String -> [String]
layers size image = go image where
    go :: String -> [String]
    go [] = []
    go image = layer : go image'
        where (layer, image') = splitAt size image
-- quickest version to write, non tail recursive though
-- folding  goes the wrong way; maybe traversing from the end is good?
-- entirely unimportant for this problem, this works plenty well


fullestLayer :: Int -> String -> String
fullestLayer size image = minimumWith (amount '0') (layers size image)


horatio :: String -> Int
horatio layer = (amount '1' layer) * (amount '2' layer)


decode :: Int -> String -> String
decode size image = foldl1' (\acc new -> zipWith stack acc new) (layers size image)


stack :: Char -> Char -> Char
stack a b = case (a, b) of
    ('0', _) -> '0'
    ('1', _) -> '1'
    ('2', _) -> b



dimensions :: (Int, Int)
dimensions = (25, 6) -- part of the puzzle input really, but hardcoding

layerSize :: Int
layerSize = (fst dimensions) * (snd dimensions)


printImage :: String -> IO ()
printImage [] = return ()
printImage image = do
    putStrLn $ map display $ take (fst dimensions) image
    printImage $ drop (fst dimensions) image


display :: Char -> Char
display c
    | c == '0' = ' '
    | c == '1' = '#'
    | otherwise = '?'



mainA :: IO ()
mainA = do
    (Just image) <- parseInput (many digitChar) "08/input.txt" 
    let answer = horatio . (fullestLayer layerSize) $ image
    print answer
    -- result <- submitAnswer 2019 08 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just image) <- parseInput (many digitChar) "08/input.txt" 
    let answer = decode layerSize image
    printImage answer
    -- print answer
    -- result <- submitAnswer 2019 08 2 answer
    -- print result
    return ()
