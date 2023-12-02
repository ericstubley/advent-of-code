module Puzzle_2023_01 where

import Automation (submitAnswer)
import Data.Char (digitToInt, isDigit)
import Data.Maybe
import Parsing

-- data types

-- parsing
-- catMaybes :: [Maybe a] -> [a] is what you want
lineP :: Parser (Maybe Int) -> Parser Int
lineP extractor = do
    digits <- catMaybes <$> many extractor
    let a = head digits
    let b = last digits
    return $ 10*a + b

amendmentP :: Parser (Maybe Int)
amendmentP = lowerChar >> return Nothing

digitP :: Parser (Maybe Int)
digitP = Just . digitToInt <$> digitChar

wordP :: String -> Int -> Parser (Maybe Int)
wordP s n = do
    lookAhead (string s)
    char (head s)
    return (Just n)

wordsP :: Parser (Maybe Int)
wordsP = choice $ map (uncurry wordP) dataEntry
  where dataEntry = zip spelled numbers
        spelled = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
        numbers = [1..9]

simpleExtractorP :: Parser (Maybe Int)
simpleExtractorP = digitP <|> amendmentP

extractorP :: Parser (Maybe Int)
extractorP = wordsP <|> simpleExtractorP

simpleCalibrationsP :: Parser [Int]
simpleCalibrationsP = sepBy (lineP simpleExtractorP) newline

calibrationsP :: Parser [Int]
calibrationsP = sepBy (lineP extractorP) newline

-- functions

-- mains

mainA :: IO ()
mainA = do
    (Just calibrationValues) <- parseInput simpleCalibrationsP "01/input.txt"
    let answer = sum calibrationValues
    print answer
    -- result <- submitAnswer 2023 01 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just calibrationValues) <- parseInput calibrationsP "01/input.txt"
    let answer = sum calibrationValues
    print answer
    -- result <- submitAnswer 2023 01 2 answer
    -- print result
    return ()
