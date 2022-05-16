module Puzzle_2019_18 where

import Automation (submitAnswer)
import Grid
import Parsing
import Control.Monad.Reader
import Data.Char (isLower, isUpper, toLower, toUpper)
import Data.Heap (Heap, Entry(..))
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Sequence (Seq(..), (><))
import qualified Data.Massiv.Array as A
import qualified Data.Heap as H
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Sequence as Sq


-- data types
data Env = Env
    { maze :: Array2 Char
    , keys :: Map Char Ix2
    , doors :: Map Char Ix2
    , start :: Ix2 } 
    deriving (Eq, Ord, Show)


type HeapEntry = Entry Int (Set Char, Ix2)


-- parsing
mazeP :: Parser (Array2 Char)
mazeP = do
    cs <- many asciiChar
    return $ A.fromLists' A.Seq (lines cs)


-- functions

buildEnvironment :: Array2 Char -> Env
buildEnvironment grid = Env grid ks ds s where
    (ks, ds, s) = A.ifoldlS extractor (M.empty, M.empty, (0:.0)) grid
    extractor (k, d, p) (i:.j) c
        | isLower c = (M.insert c (i:.j) k, d, p)
        | isUpper c = (k, M.insert c (i:.j) d, p)
        | c == '@'  = (k, d, (i:.j))
        | otherwise = (k, d, p)


neighbours :: Ix2 -> [Ix2]
neighbours (i:.j) = [(i+1):.j, (i-1):.j, i:.(j+1), i:.(j-1)]


tileAt :: Env -> Ix2 -> Char
tileAt env pos = maze env A.! pos


-- given a grid, find the smallest number of steps needed to collect the keys
findKeys :: Array2 Char -> Int
findKeys grid = explore env initHeap where
    env = buildEnvironment grid
    initHeap = (H.singleton (Entry 0 (S.empty, start env)))


-- in a environment, with a heap of distance (keys, position) either exit if
-- we've found all the keys or keep going based on the current options
explore :: Env -> Heap HeapEntry -> Int
explore env heap 
    | S.size ks == n = d
    | otherwise      = explore env heap'
    where
        (Entry d (ks, pos)) = H.minimum heap
        n = M.size (keys env)
        opts = formatOptions env d ks pos
        heap' = H.union (H.deleteMin heap) (H.fromList opts)


formatOptions :: Env -> Int -> Set Char -> Ix2 -> [HeapEntry]
formatOptions env d ks pos = map formatter (options env ks pos) where
    formatter :: (Int, (Char, Ix2)) -> HeapEntry
    formatter (d', (c, pos')) = Entry (d+d') (S.insert c ks, pos')



-- given a set of keys and a position, find the distances to all keys which
-- can be reached without passing through locked doors or over other keys
options :: Env -> Set Char -> Ix2 -> [(Int, (Char, Ix2))]
options env ks pos = go (Sq.singleton (pos, 0)) (S.empty) [] where
    go :: Seq (Ix2, Int) -> Set Ix2 -> [(Int, (Char, Ix2))] -> [(Int, (Char, Ix2))]
    go Empty _ found = found
    go ((p,d):<|qs) seen found
        | S.member p seen = go qs seen found
        | isLower c && (not $ S.member c ks) = go qs seen' found'
        | isLower c = go qs' seen' found
        | isUpper c && (S.member (toLower c) ks) = go qs' seen' found
        | c == '.' || c == '@' = go qs' seen' found
        | otherwise = go qs seen' found
        where
            c = tileAt env p
            qs' = qs >< (Sq.fromList $ zip (neighbours p) (repeat (d+1)))
            seen' = S.insert p seen
            found' = found ++ [(d, (c, p))]



-- mains

mainA :: IO ()
mainA = do
    (Just grid) <- parseInput mazeP "18/input.txt"
    let answer = findKeys grid
    print answer
    -- result <- submitAnswer 2019 18 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    let answer = 0
    print answer
    -- result <- submitAnswer 2019 18 2 answer
    -- print result
    return ()
