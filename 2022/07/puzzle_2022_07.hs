module Puzzle_2022_07 where

import Automation (submitAnswer)
import Parsing

-- data types
data Object = File String Int | Directory String [Object]
    deriving (Eq, Show)

-- parsing
commandP :: Parser Object
commandP = do
    name <- cdDirP
    newline
    lsP 
    newline
    objects <- manyTill (objectP <* optional newline) (eof <|> cdUpP)
    return $ Directory name objects

objectP :: Parser Object
objectP = fileP <|> commandP <|> (dirP >> newline >> objectP)

cdDirP :: Parser String
cdDirP = string "$ cd " >> nameP

cdUpP :: Parser ()
cdUpP = string "$ cd .." >> return ()

lsP :: Parser ()
lsP = string "$ ls" >> return ()

dirP :: Parser String
dirP = string "dir " >> nameP

fileP :: Parser Object
fileP = do
    size <- natural
    space
    name <- nameP
    return $ File name size 


nameP :: Parser String
nameP = some (lowerChar <|> char '.' <|> char '/')

-- functions

totalSize :: Object -> Int
totalSize (File _ size) = size
totalSize (Directory _ objects) = sum $ map totalSize objects


sizes :: Object -> [Int]
sizes (File _ _) = []
sizes (Directory name objects) = size : subSizes where
    size = totalSize (Directory name objects)
    subSizes = concat $ map sizes objects


totalThresholdSize :: Int -> Object -> Int
totalThresholdSize threshold = sum . filter (<= threshold) . sizes


select :: Int -> Int -> Object -> Int
select total needed root = minimum . filter ( >= threshold) . sizes $ root
  where
    free = total - (totalSize root)
    threshold = needed - free

-- mains

mainA :: IO ()
mainA = do
    (Just root) <- parseInput commandP "07/input.txt"
    let answer = totalThresholdSize 100000 root
    print answer
    -- result <- submitAnswer 2022 07 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just root) <- parseInput commandP "07/input.txt"
    let answer = select 70000000 30000000 root
    print answer
    -- result <- submitAnswer 2022 07 2 answer
    -- print result
    return ()
