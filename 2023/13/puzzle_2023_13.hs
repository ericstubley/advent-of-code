module Puzzle_2023_13 where

import Automation (submitAnswer)
import Parsing
import Data.Massiv.Array (Array, Ix1(..), Ix2(..), Sz(..), (<!), (!>))
import qualified Data.Massiv.Array as A
import Data.Maybe
import Control.Monad.Reader

-- data types
type Array1 a = A.Array A.U A.Ix1 a
type Line = Array1 Char
type Array2 a = A.Array A.U A.Ix2 a
type Pattern = Array2 Char

-- parsing
patternsP :: Parser [Pattern]
patternsP = sepBy patternP newline

patternP :: Parser Pattern
patternP = A.fromLists' A.Seq <$> sepEndBy (some rockP) newline

rockP :: Parser Char
rockP = char '.' <|> char '#'

-- functions
summarize :: Int -> [Pattern] -> Int
summarize errors patterns = 100*h + v
  where v = sum . catMaybes . map (\p -> runReader (vertical p) errors) $ patterns
        h = sum . catMaybes . map (\p -> runReader (horizontal p) errors) $ patterns

summarizeA :: [Pattern] -> Int
summarizeA = summarize 0

summarizeB :: [Pattern] -> Int
summarizeB = summarize 1

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:xs) = Just x

off :: (Line, Line) -> Int
off (l1, l2) = A.sum $ A.zipWith (\x y -> if x/=y then 1 else 0) l1 l2

vertical :: MonadReader Int m => Pattern -> m (Maybe Int)
vertical pattern = do
    let (Sz (r :. c)) = A.size pattern
    errors <- ask 
    return . head' . filter (\i -> countVertical pattern i == errors) $ [1..(c-1)]

countVertical :: Pattern -> Int -> Int
countVertical pattern i = sum . map off $ paired
  where (Sz (r :. c)) = A.size pattern
        paired = [ (A.compute $ pattern <! (i-j-1), A.compute $ pattern <! (i+j))
                 | j <- [0..c]
                 , (i-j-1) >= 0
                 , (i+j) <= (c-1)]

horizontal :: MonadReader Int m => Pattern -> m (Maybe Int)
horizontal pattern = do
    let (Sz (r :. c)) = A.size pattern
    errors <- ask 
    return . head' . filter (\i -> countHorizontal pattern i == errors) $ [1..(r-1)]

countHorizontal :: Pattern -> Int -> Int
countHorizontal pattern i = sum . map off $ paired
  where (Sz (r :. c)) = A.size pattern
        paired = [ (pattern !> (i-j-1), pattern !> (i+j))
                 | j <- [0..r]
                 , (i-j-1) >= 0
                 , (i+j) <= (r-1)]
-- mains

mainA :: IO ()
mainA = do
    (Just patterns) <- parseInput patternsP "13/input.txt"
    let answer = summarizeA patterns
    print answer
    -- result <- submitAnswer 2023 13 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just patterns) <- parseInput patternsP "13/input.txt"
    let answer = summarizeB patterns
    print answer
    -- result <- submitAnswer 2023 13 2 answer
    -- print result
    return ()
