module Puzzle_2023_01 where

import Automation (submitAnswer)
import Data.Char (digitToInt, isDigit)
import Parsing

-- data types

-- parsing
calibrationsP :: Parser [Int]
calibrationsP = sepBy calibrationP newline

calibrationP :: Parser Int
calibrationP = do
    digits <- digitsP
    let a = head digits
    let b = last digits
    return $ 10*a + b

digitsP :: Parser [Int]
digitsP = amendmentsP *> many (digitP <* amendmentsP)

digitP :: Parser Int
digitP = fmap (digitToInt) digitChar

amendmentsP :: Parser ()
amendmentsP = skipMany lowerChar

actualCalibrationsP :: Parser [Int]
actualCalibrationsP = sepBy actualCalibrationP newline

actualCalibrationP :: Parser Int
actualCalibrationP = do
    digits <- actualDigitsP
    let a = head digits
    let b = last digits
    return $ 10*a + b

actualDigitsP :: Parser [Int]
actualDigitsP = fmap (map digitToInt . filter isDigit) actualCharsP

actualCharsP :: Parser [Char]
actualCharsP = many (digitChar <|> spelledAndAdvanceP <|> lowerChar)

spelledAndAdvanceP :: Parser Char
spelledAndAdvanceP = lookAhead spelledP <* lowerChar

spelledP :: Parser Char
spelledP = (string "zero" >> return '0')
    <|> (string "one"     >> return '1')
    <|> (string "two"     >> return '2')
    <|> (string "three"   >> return '3')
    <|> (string "four"    >> return '4')
    <|> (string "five"    >> return '5')
    <|> (string "six"     >> return '6')
    <|> (string "seven"   >> return '7')
    <|> (string "eight"   >> return '8')
    <|> (string "nine"    >> return '9')

-- efnostz

-- functions

-- mains

mainA :: IO ()
mainA = do
    (Just calibrationValues) <- parseInput calibrationsP "01/input.txt"
    let answer = sum calibrationValues
    print answer
    -- result <- submitAnswer 2023 01 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just calibrationValues) <- parseInput actualCalibrationsP "01/input.txt"
    let answer = sum calibrationValues
    print answer
    -- result <- submitAnswer 2023 01 2 answer
    -- print result
    return ()
