module Puzzle_2023_07 where

import Automation (submitAnswer)
import Parsing
import Data.Sort (sort, sortOn)
import Data.Map.Strict (Map)

import qualified Data.Map.Strict as M

-- data types
data Card = Joker
          | One
          | Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          | Ace
            deriving (Eq, Ord, Show)

data Type = HighCard 
          | OnePair 
          | TwoPair 
          | ThreeOfAKind 
          | FullHouse 
          | FourOfAKind 
          | FiveOfAKind
            deriving (Eq, Ord, Show)

data Hand = Hand { _cards :: (Card, Card, Card, Card, Card)
                 , _bid :: Int }


-- parsing

handsP :: Parser Card -> Parser [Hand]
handsP jP = sepBy (handP jP) newline

handsPA :: Parser [Hand]
handsPA = handsP jackP

handsPB :: Parser [Hand]
handsPB = handsP jokerP

handP :: Parser Card -> Parser Hand
handP jP = do
    cards <- cardsP jP
    space
    bid <- natural
    return $ Hand cards bid

cardsP :: Parser Card -> Parser (Card, Card, Card, Card, Card)
cardsP jP = do
    a <- cardP jP
    b <- cardP jP
    c <- cardP jP
    d <- cardP jP
    e <- cardP jP
    return (a, b, c, d, e)

cardP :: Parser Card -> Parser Card
cardP jP = jP
    <|> (char '1' >> return One)
    <|> (char '2' >> return Two)
    <|> (char '3' >> return Three)
    <|> (char '4' >> return Four)
    <|> (char '5' >> return Five)
    <|> (char '6' >> return Six)
    <|> (char '7' >> return Seven)
    <|> (char '8' >> return Eight)
    <|> (char '9' >> return Nine)
    <|> (char 'T' >> return Ten)
    <|> (char 'Q' >> return Queen)
    <|> (char 'K' >> return King)
    <|> (char 'A' >> return Ace)

jackP :: Parser Card
jackP  = char 'J' >> return Jack

jokerP :: Parser Card
jokerP  = char 'J' >> return Joker

-- functions
handType :: Hand -> Type
handType hand
    | c == [5] = FiveOfAKind
    | c == [1, 4] = FourOfAKind
    | c == [2, 3] = FullHouse
    | c == [1, 1, 3] = ThreeOfAKind
    | c == [1, 2, 2] = TwoPair
    | c == [1, 1, 1, 2] = OnePair
    | c == [1, 1, 1, 1, 1] = HighCard
      where c = counts $ palmistry hand

bestType :: Hand -> Type
bestType hand
    | j == 0 = handType hand
    | j == 1 && c' == [1, 1, 1, 1] = OnePair
    | j == 1 && c' == [1, 1, 2] = ThreeOfAKind
    | j == 1 && c' == [2, 2] = FullHouse
    | j == 1 && c' == [1, 3] = FourOfAKind
    | j == 1 && c' == [4] = FiveOfAKind
    | j == 2 && c' == [1, 1, 1] = ThreeOfAKind
    | j == 2 && c' == [1, 2] = FourOfAKind
    | j == 2 && c' == [3] = FiveOfAKind
    | j == 3 && c' == [1, 1] = FourOfAKind
    | j == 3 && c' == [2] = FiveOfAKind
    | j == 4 = FiveOfAKind
    | j == 5 = FiveOfAKind
      where p = palmistry hand
            p' = M.delete Joker p
            c' = counts p'
            j = M.findWithDefault 0 Joker p 

counts :: Map Card Int -> [Int]
counts palm = sort . M.elems $ palm

palmistry :: Hand -> Map Card Int
palmistry hand = M.unionsWith (+) $ singles
  where (a, b, c, d, e) = _cards hand
        singles = map (\x -> M.singleton x 1) [a, b, c, d, e]

handOrderA :: Hand -> (Type, (Card, Card, Card, Card, Card))
handOrderA hand = (handType hand, _cards hand)

handOrderB :: Hand -> (Type, (Card, Card, Card, Card, Card))
handOrderB hand = (bestType hand, _cards hand)

winnings :: Ord a => (Hand -> a) -> [Hand] -> Int
winnings orderer = sum . zipWith (*) [1..] . map _bid . sortOn orderer

winningsA :: [Hand] -> Int
winningsA = winnings handOrderA

winningsB :: [Hand] -> Int
winningsB = winnings handOrderB

-- mains

mainA :: IO ()
mainA = do
    (Just hands) <- parseInput handsPA "07/input.txt"
    let answer = winningsA hands
    print answer
    -- result <- submitAnswer 2023 07 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just hands) <- parseInput handsPB "07/input.txt"
    let answer = winningsB hands
    print answer
    -- result <- submitAnswer 2023 07 2 answer
    -- print result
    return ()
