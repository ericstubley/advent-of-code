module Puzzle_2020_02 where

import Automation (submitAnswer)
import Parsing

-- data types
data Rule = Rule
    { lower :: Int
    , upper :: Int
    , letter :: Char}
    deriving (Eq, Ord, Show)

type Password = String

-- parsing
databaseP :: Parser [(Rule, Password)]
databaseP = sepBy entryP newline

entryP :: Parser (Rule, Password)
entryP = do
    m <- integer
    char '-'
    n <- integer
    char ' '
    c <- lowerChar
    string ": "
    pwd <- some lowerChar

    return (Rule m n c, pwd)

-- functions
isValid :: Rule -> Password -> Bool
isValid r pwd = lower r <= occurences && occurences <= upper r
  where occurences = length $ filter (== letter r) pwd 


isPositional :: Rule -> Password -> Bool
isPositional (Rule l u c) pwd
    | length pwd < u = False 
    | otherwise      = xor lCond rCond
      where lCond = pwd !! (l-1) == c
            rCond = pwd !! (u-1) == c


xor :: Bool -> Bool -> Bool
xor a b = a /= b


countValid :: (Rule -> Password -> Bool) -> [(Rule, Password)] -> Int
countValid validator db = length $ filter (\e -> validator (fst e) (snd e)) db

-- mains

mainA :: IO ()
mainA = do
    (Just database) <- parseInput databaseP "02/input.txt"
    let answer = countValid isValid database
    print answer
    -- result <- submitAnswer 2020 02 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just database) <- parseInput databaseP "02/input.txt"
    let answer = countValid isPositional database
    print answer
    -- result <- submitAnswer 2020 02 2 answer
    -- print result
    return ()
