module Puzzle_2020_11 where

import Automation (submitAnswer)
import Parsing
import Grid
import qualified Data.Massiv.Array as A


-- parsing
seatsP :: Parser (Array2 Char)
seatsP = A.fromLists' A.Seq <$> sepBy rowP newline

rowP :: Parser [Char]
rowP = some seatP

seatP :: Parser Char
seatP = char '.' <|> char 'L' <|> char '#'

-- functions
-- is there a more intrinsic way to do this???
isOccupied :: Char -> Int
isOccupied c = if c == '#' then 1 else 0


countOccupied :: Array2 Char -> Int
countOccupied room = A.foldlS (\acc c -> acc + isOccupied c) 0 room


adjacent :: [Ix2]
adjacent = [ -1 :. -1, 0 :. -1, 1 :. -1
           , -1 :.  0,          1 :.  0
           , -1 :.  1, 0 :.  1, 1 :.  1]


neighbourStencil :: A.Stencil Ix2 Char Char
neighbourStencil = A.makeStencil (A.Sz (3:.3)) (1:.1) go
  where go :: (Ix2 -> Char) -> Char
        go get = decision (get (0:.0)) (sum $ map (isOccupied . get) adjacent)


decision :: Char -> Int -> Char
decision c n 
    | c == 'L' && n == 0 = '#'
    | c == '#' && n >= 4 = 'L'
    | otherwise          =  c



step :: Array2 Char -> Array2 Char
step room = A.compute $ A.mapStencil (A.Fill '.') neighbourStencil room


stableState :: Array2 Char -> Array2 Char
stableState room = A.iterateUntil 
    (\x a1 a2 -> a1 == a2) -- convergence condition, first arg is step counter
    (\x a -> step a) -- modification at iteration, first arg is step counter
    room


-- part B, very similar
-- hardcoded very large stencil
neighbourStencil' :: A.Stencil Ix2 Char Char
neighbourStencil' = A.makeStencil (A.Sz (200:.200)) (1:.1) go
  where go :: (Ix2 -> Char) -> Char
        go get = decision' (get (0:.0)) 
                    (sum $ map (isOccupied . (look get)) adjacent)


-- hardcoded 100 maximum look distance
look :: (Ix2 -> Char) -> Ix2 -> Char
look get (di:.dj)
    | length seen == 0 = '.'
    | otherwise        = head seen
      where ray = [n*di :. n*dj | n <- [1..100]]
            seen = dropWhile (=='.') (map get ray)


decision' :: Char -> Int -> Char
decision' c n 
    | c == 'L' && n == 0 = '#'
    | c == '#' && n >= 5 = 'L'
    | otherwise          =  c


step' :: Array2 Char -> Array2 Char
step' room = A.compute $ A.mapStencil (A.Fill '.') neighbourStencil' room


stableState' :: Array2 Char -> Array2 Char
stableState' room = A.iterateUntil 
    (\x a1 a2 -> a1 == a2) -- convergence condition, first arg is step counter
    (\x a -> step' a) -- modification at iteration, first arg is step counter
    room

-- mains

mainA :: IO ()
mainA = do
    (Just waitingRoom) <- parseInput seatsP "11/input.txt"
    let answer = countOccupied $ stableState waitingRoom
    print answer
    -- result <- submitAnswer 2020 11 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just waitingRoom) <- parseInput seatsP "11/input.txt"
    let answer = countOccupied $ stableState' waitingRoom
    print answer
    -- result <- submitAnswer 2020 11 2 answer
    -- print result
    return ()
