module Puzzle_2023_04 where

import Automation (submitAnswer)
import Parsing
import Data.Bits (shiftL)
import Data.Set (Set)

import qualified Data.Set as S

-- data types
data Card = Card
    { winners :: Set Int
    , numbers :: Set Int
    } deriving (Eq, Ord, Show)

-- parsing
cardsP :: Parser [Card]
cardsP = sepBy cardP newline

cardP :: Parser Card
cardP = do
    string "Card"
    space
    natural
    char ':'
    ws <- setP
    string " | "
    ns <- setP
    return (Card ws ns)

setP :: Parser (Set Int)
setP = S.fromList <$> many (try $ hspace *> natural)

-- functions
score :: Card -> Int
score card
    | wc == 0   = 0
    | otherwise = shiftL 1 $ wc - 1
      where wc = winCount card

winCount :: Card -> Int
winCount card = S.size $ S.intersection (winners card) (numbers card)


countCards :: [Card] -> [Int]
countCards cards = count' cards (repeat 1)
  where count' [] _ = []
        count' (x:xs) tracking = mult : count' xs updated
          where mult = (head tracking)
                dist = winCount x
                near = zipWith (+) (take dist (tail tracking)) (repeat mult)
                far = drop dist (tail tracking)
                updated = near ++ far

-- mains

mainA :: IO ()
mainA = do
    (Just cards) <- parseInput cardsP "04/input.txt"
    let answer = sum $ map score cards
    print answer
    -- result <- submitAnswer 2023 04 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just cards) <- parseInput cardsP "04/input.txt"
    let answer = sum $ countCards cards
    print answer
    -- result <- submitAnswer 2023 04 2 answer
    -- print result
    return ()
