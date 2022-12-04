module Puzzle_2022_04 where

import Automation (submitAnswer)
import Parsing hiding (count)

-- data types
data Assignment = Assignment 
    { _l1 :: Int
    , _u1 :: Int
    , _l2 :: Int
    , _u2 :: Int } deriving (Eq, Ord, Show)

-- parsing
assignmentsP :: Parser [Assignment]
assignmentsP = sepBy assignmentP newline

assignmentP :: Parser Assignment
assignmentP = do
    l1 <- natural
    char '-'
    u1 <- natural
    char ','
    l2 <- natural
    char '-'
    u2 <- natural
    return $ Assignment l1 u1 l2 u2


-- functions
-- test if one pair contains the other
containment :: Assignment -> Bool
containment a
    | (_l1 a <= _l2 a) && (_u1 a >= _u2 a) = True
    | (_l1 a >= _l2 a) && (_u1 a <= _u2 a) = True
    | otherwise                            = False


count :: (a -> Bool) -> [a] -> Int
count f xs = length $ filter f xs


-- test if there's any overlap at all
overlap :: Assignment -> Bool
overlap a
    | (_l1 a <= _u2 a) && (_l2 a <= _u1 a) = True
    | otherwise                            = False



-- mains

mainA :: IO ()
mainA = do
    (Just assignments) <- parseInput assignmentsP "04/input.txt"
    let answer = count containment assignments
    print answer
    -- result <- submitAnswer 2022 04 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just assignments) <- parseInput assignmentsP "04/input.txt"
    let answer = count overlap assignments
    print answer
    result <- submitAnswer 2022 04 2 answer
    print result
    return ()
