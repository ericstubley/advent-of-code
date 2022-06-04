module Puzzle_2020_05 where

import Automation (submitAnswer)
import Parsing
import Data.Sort (sort)

-- parsing
seatsP :: Parser [Int]
seatsP = sepBy passP newline

passP :: Parser Int
passP = seatID . (map digit) <$> scanP

scanP :: Parser [Char]
scanP = do
    row <- count 7 (char 'F' <|> char 'B')
    col <- count 3 (char 'L' <|> char 'R')
    return $ row ++ col


-- functions
seatID :: [Int] -> Int
seatID xs = foldl (\acc x -> 2*acc + x) 0 xs


digit :: Char -> Int
digit c = case c of
    'F' -> 0
    'B' -> 1
    'L' -> 0
    'R' -> 1
    _   -> error "Unexpected digit"


findSeat :: [Int] -> Int
findSeat [] = error "Ran out of seats!!!"
findSeat (x:xs@(y:_))
    | y == x + 1 = findSeat xs
    | otherwise  = x+1

-- mains

mainA :: IO ()
mainA = do
    (Just seats) <- parseInput seatsP "05/input.txt"
    let answer = maximum seats
    print answer
    -- result <- submitAnswer 2020 05 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just seats) <- parseInput seatsP "05/input.txt"
    let answer = findSeat $ sort seats
    print answer
    -- result <- submitAnswer 2020 05 2 answer
    -- print result
    return ()
