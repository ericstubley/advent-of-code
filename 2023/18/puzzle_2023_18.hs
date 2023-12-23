module Puzzle_2023_18 where

import Automation (submitAnswer)
import Parsing
import Grid (Direction(..))
import Data.Char (digitToInt)
import Data.List (nub)
import Data.Massiv.Array (Ix2(..))

-- data types
data Move = Move { _heading :: Direction
                 , _length :: Int } deriving (Eq, Ord, Show)

-- parsing
planAP :: Parser [Move]
planAP = sepBy moveAP newline

moveAP :: Parser Move
moveAP = do
    d <- dirP
    space 
    l <- natural
    string " (#"
    count 6 hexDigitChar
    string ")"
    return $ Move d l

dirP :: Parser Direction
dirP = (char 'U' >> return North)
   <|> (char 'R' >> return East)
   <|> (char 'D' >> return South)
   <|> (char 'L' >> return West) 

planBP :: Parser [Move]
planBP = sepBy moveBP newline

moveBP :: Parser Move
moveBP = do
    dirP
    space
    natural
    string " (#"
    l <- martian <$> count 5 hexDigitChar
    d <- colourP
    string ")"
    return $ Move d l

-- 0 means R, 1 means D, 2 means L, and 3 means U
colourP :: Parser Direction
colourP = (char '0' >> return East)
      <|> (char '1' >> return South)
      <|> (char '2' >> return West)
      <|> (char '3' >> return North)

martian :: String -> Int
martian ss = foldl (\acc x -> 16*acc + x) 0 $ map digitToInt ss

-- functions

follow :: [Move] -> [Ix2]
follow moves = go (0:.0) moves
  where go _ [] = []
        go ix ((Move d l):ms) = ix' : go ix' ms
              where ix' = moveWithDist l d ix


moveWithDist :: Int -> Direction -> Ix2 -> Ix2
moveWithDist d North (i:.j) = (i-d) :. j
moveWithDist d East  (i:.j) = i :. (j+d)
moveWithDist d South (i:.j) = (i+d) :. j
moveWithDist d West  (i:.j) = i :. (j-d)


shoelace :: [Ix2] -> Int
shoelace xs = div (abs $ shoelace' xs) 2

shoelace' :: [Ix2] -> Int
shoelace' [] = 0
shoelace' (x:[]) = 0
shoelace' (x:xs@(y:ys)) = (a1*b2 - a2*b1) + shoelace' xs
  where (a1 :. b1) = x
        (a2 :. b2) = y


area :: [Move] -> Int
area plan = div (4*a + 2*l + 4) 4
  where path = ((0:.0) :) . follow $ plan
        a = shoelace path
        l = sum . map (uncurry manhattan) $ zip path (tail path)


manhattan :: Ix2 -> Ix2 -> Int
manhattan (a1:.b1) (a2:.b2) = abs (a1 - a2) + abs (b1 - b2)



-- mains

mainA :: IO ()
mainA = do
    (Just planA) <- parseInput planAP "18/input.txt"
    let answer = area planA
    print answer
    -- result <- submitAnswer 2023 18 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just planB) <- parseInput planBP "18/input.txt"
    let answer = area planB
    print answer
    -- result <- submitAnswer 2023 18 2 answer
    -- print result
    return ()
