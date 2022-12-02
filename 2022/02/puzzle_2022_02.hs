module Puzzle_2022_02 where

import Automation (submitAnswer)
import Parsing
import Utilities (CyclicEnum(..))

-- data types
data Shape = Rock | Paper | Scissors 
    deriving (Ord, Eq, Enum, Bounded)
instance CyclicEnum Shape
data Outcome = Lose | Draw | Win
    deriving (Eq, Enum, Bounded)
type Game = (Shape, Shape)
type Strategy = (Shape, Outcome)

-- parsing
guideP :: Parser [Game]
guideP = sepBy gameP newline


gameP :: Parser Game
gameP = do
    elf <- char 'A' <|> char 'B' <|> char 'C'
    char ' '
    you <- char 'X' <|> char 'Y' <|> char 'Z'
    return (decryptShape elf, decryptShape you)


decryptShape :: Char -> Shape
decryptShape x = case x of
                    'A' -> Rock
                    'B' -> Paper
                    'C' -> Scissors
                    'X' -> Rock
                    'Y' -> Paper
                    'Z' -> Scissors
                    _ -> error "Unexpected decryption"


advancedGuideP :: Parser [Strategy]
advancedGuideP = sepBy strategyP newline


strategyP :: Parser Strategy
strategyP = do
    elf <- char 'A' <|> char 'B' <|> char 'C'
    char ' '
    you <- char 'X' <|> char 'Y' <|> char 'Z'
    return (decryptShape elf, decryptOutcome you)


decryptOutcome :: Char -> Outcome
decryptOutcome x = case x of
                    'X' -> Lose
                    'Y' -> Draw
                    'Z' -> Win
                    _ -> error "Unexpected decryption"

-- functions
scoreGuide :: [Game] -> Int
scoreGuide guide = sum $ map score guide


score :: Game -> Int
score game = (scoreShape $ snd game) + (scoreOutcome outcome)
  where outcome = computeOutcome game


computeOutcome :: Game -> Outcome
computeOutcome (elf, you)
    | elf == you       = Draw
    | you == csucc elf = Win
    | otherwise        = Lose


scoreShape :: Shape -> Int
scoreShape Rock = 1
scoreShape Paper = 2
scoreShape Scissors = 3


scoreOutcome :: Outcome -> Int
scoreOutcome Lose = 0
scoreOutcome Draw = 3
scoreOutcome Win = 6


advancedScoreGuide :: [Strategy] -> Int
advancedScoreGuide guide = sum $ map advancedScore guide


advancedScore :: Strategy -> Int
advancedScore strat = (scoreShape shape) + (scoreOutcome $ snd strat)
  where shape = computeShape strat


computeShape :: Strategy -> Shape
computeShape (play, result)
    | result == Draw = play
    | result == Win  = csucc play
    | otherwise      = cpred play

-- mains

mainA :: IO ()
mainA = do
    (Just guide) <- parseInput guideP "02/input.txt"
    let answer = scoreGuide guide
    print answer
    -- result <- submitAnswer 2022 02 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just guide) <- parseInput advancedGuideP "02/input.txt"
    let answer = advancedScoreGuide guide
    -- result <- submitAnswer 2022 02 2 answer
    -- print result
    return ()
