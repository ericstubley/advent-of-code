module Puzzle_2020_22 where

import Automation (submitAnswer)
import Parsing
import Data.Foldable (toList)
import Data.Sequence (Seq(..), (|>))
import Data.Set (Set)
import qualified Data.Sequence as S
import qualified Data.Set as Se

-- data types
type Deck = Seq Int

data Player = One | Two deriving (Eq, Ord, Show)

-- parsing
decksP :: Parser (Deck, Deck)
decksP = do
    string "Player 1:\n"
    d1 <- some cardP
    string "\nPlayer 2:\n"
    d2 <- some cardP

    return (S.fromList d1, S.fromList d2)


cardP :: Parser Int
cardP = integer <* optional newline


-- functions
combat :: Deck -> Deck -> Deck
combat Empty d2 = d2
combat d1 Empty = d1
combat (a:<|d1) (b:<|d2)
    | a > b = combat (d1 |> a |> b) d2
    | a < b = combat d1 (d2 |> b |> a)


score :: Deck -> Int
score deck = sum $ zipWith (*) [1..] (toList . S.reverse $ deck)


-- let's try the non-memoized version first
recursiveCombat :: Deck -> Deck -> Deck
recursiveCombat d1 d2 = winningDeck
  where (winner, winningDeck) = go Se.empty d1 d2
        go :: Set (Deck, Deck) -> Deck -> Deck -> (Player, Deck)
        go _ Empty d2 = (Two, d2)
        go _ d1 Empty = (One, d1)
        go s d1@(a:<|d1') d2@(b:<|d2')
            | Se.member (d1, d2) s        = (One, d1)
            | recurse && subWinner == One = go s' (d1' |> a |> b) d2'
            | recurse && subWinner == Two = go s' d1' (d2' |> b |> a)
            | a > b                       = go s' (d1' |> a |> b) d2'
            | a < b                       = go s' d1' (d2' |> b |> a)
              where recurse = a <= S.length d1' && b <= S.length d2'
                    (subWinner, _) = go Se.empty (S.take a d1') (S.take b d2')
                    s' = Se.insert (d1, d2) s

-- mains

mainA :: IO ()
mainA = do
    (Just (deck1, deck2)) <- parseInput decksP "22/input.txt"
    let answer = score $ combat deck1 deck2
    print answer
    -- result <- submitAnswer 2020 22 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just (deck1, deck2)) <- parseInput decksP "22/input.txt"
    let answer = score $ recursiveCombat deck1 deck2
    print answer
    -- result <- submitAnswer 2020 22 2 answer
    -- print result
    return ()
