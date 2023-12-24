module Puzzle_2023_21 where

import Automation (submitAnswer)
import Parsing
import Utilities (repeatM, repeatThroughM)

import Control.Monad.Reader
import Data.Massiv.Array (Array, Ix2(..), Sz(..))
import Data.Set (Set)

import qualified Data.Massiv.Array as A
import qualified Data.Set as S

-- data types
type Array2 a = A.Array A.U A.Ix2 a
type Garden = Array2 Char

-- parsing
gardenP :: Parser Garden
gardenP = A.fromLists' A.Seq <$> sepBy (some squareP) newline

squareP :: Parser Char
squareP = char '.' <|> char '#' <|> char 'S'

-- functions
steps :: Garden -> Int -> Int
steps garden n = S.size . last $ runReader (repeatThroughM n walk start) garden
  where start = S.singleton $ findElf garden


findElf :: Garden -> Ix2
findElf garden = head . filter (\ix -> (garden A.! ix) == 'S') $ grid
  where size = A.size garden
        (A.Sz (h:.w)) = size
        grid = [i:.j | i <- [0..(h-1)]
                     , j <- [0..(w-1)]]


walk :: MonadReader Garden m => Set Ix2 -> m (Set Ix2)
walk input = S.fromList <$> go (S.toList input)
  where go :: MonadReader Garden m => [Ix2] -> m [Ix2]
        go [] = return []
        go (s:ss) = do
            options <- neighbours s
            rest <- go ss
            return $ options ++ rest


neighbours :: MonadReader Garden m => Ix2 -> m [Ix2]
neighbours ix = do
    let candidates = ns ix
    filterM isValid candidates


isValid :: MonadReader Garden m => Ix2 -> m Bool
isValid ix = do
    a <- inBounds ix
    b <- walkable ix
    return (a && b)


inBounds :: MonadReader Garden m => Ix2 -> m Bool
inBounds ix = do
    s <- reader A.size
    return $ A.isSafeIndex s ix


walkable :: MonadReader Garden m => Ix2 -> m Bool
walkable ix = do
    c <- reader (\g -> g A.! ix)
    return (c /= '#')


ns :: Ix2 -> [Ix2]
ns (i:.j) = [ (i+1) :. j
            , (i-1) :. j
            , i :. (j+1)
            , i :. (j-1)]

-- part b
wrapSteps :: Garden -> Int -> Int
wrapSteps garden n = S.size . last $ runReader (repeatThroughM n wrapWalk start) garden
  where start = S.singleton $ findElf garden

wrapWalk :: MonadReader Garden m => Set Ix2 -> m (Set Ix2)
wrapWalk input = S.fromList <$> go (S.toList input)
  where go :: MonadReader Garden m => [Ix2] -> m [Ix2]
        go [] = return []
        go (s:ss) = do
            options <- wrapNeighbours s
            rest <- go ss
            return $ options ++ rest

wrapNeighbours :: MonadReader Garden m => Ix2 -> m [Ix2]
wrapNeighbours ix = do
    let candidates = ns ix
    filterM wrapWalkable candidates

wrapWalkable :: MonadReader Garden m => Ix2 -> m Bool
wrapWalkable (i:.j) = do
    s <- reader A.size
    let (A.Sz (h:.w)) = s
    c <- reader (\g -> g A.! ((mod i h) :. (mod j w)))
    return (c /= '#')


-- mains

mainA :: IO ()
mainA = do
    (Just garden) <- parseInput gardenP "21/input.txt"
    let answer = steps garden 64
    print answer
    -- result <- submitAnswer 2023 21 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just garden) <- parseInput gardenP "21/input.txt"
    print (wrapSteps garden 65)
    print (wrapSteps garden (65 + 131))
    print (wrapSteps garden (65 + 131 + 131))
    -- result <- submitAnswer 2023 21 2 answer
    -- print result
    return ()
