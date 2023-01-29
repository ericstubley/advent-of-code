module Puzzle_2021_04 where

import Automation (submitAnswer)
import Parsing
import Data.List (partition)
import Data.Massiv.Array (Ix2(..))
import Data.Maybe
import qualified Data.Massiv.Array as A

-- data types
type Board = A.Array A.B A.Ix2 (Maybe Int)

-- parsing
bingoP :: Parser ([Int], [Board])
bingoP = do
    calls <- callsP
    newline
    newline
    boards <- sepBy boardP newline
    return (calls, boards)


callsP :: Parser [Int]
callsP = sepEndBy integer (char ',')


boardP :: Parser Board
boardP = do
    lists <- count 5 (lineP <* (optional newline))  
    return $ A.fromLists' A.Seq lists


lineP :: Parser [Maybe Int]
lineP = count 5 (Just <$> (space >> natural))


-- functions
mark :: Int -> Maybe Int -> Maybe Int
mark call square = do
    x <- square
    if x == call 
        then Nothing
        else (Just x)


markBoard :: Int -> Board -> Board
markBoard call board = (mark call) <$> board 


score :: Board -> Int
score board = A.sum ((fromMaybe 0) <$> board)


winning :: Board -> Bool
winning board = rows || cols
  where rows = any (all (== Nothing)) (A.outerSlices board)
        cols = any (all (== Nothing)) (A.innerSlices board)


firstScore :: [Int] -> [Board] -> Int
firstScore calls boards = head $ bingos calls boards


lastScore :: [Int] -> [Board] -> Int
lastScore calls boards = last $ bingos calls boards


bingos :: [Int] -> [Board] -> [Int]
bingos [] _ = []
bingos _ [] = []
bingos (c:cs) boards = scores ++ (bingos cs boards')
  where (wins, boards') = partition winning $ map (markBoard c) boards
        scores = map (\b -> c * (score b)) wins

-- mains

mainA :: IO ()
mainA = do
    (Just (calls, boards)) <- parseInput bingoP "04/input.txt"
    let answer = firstScore calls boards
    print answer
    -- result <- submitAnswer 2021 04 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just (calls, boards)) <- parseInput bingoP "04/input.txt"
    let answer = lastScore calls boards
    print answer
    -- result <- submitAnswer 2021 04 2 answer
    -- print result
    return ()
