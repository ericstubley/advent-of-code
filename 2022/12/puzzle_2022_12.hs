module Puzzle_2022_12 where

import Automation (submitAnswer)
import Parsing
import Data.Char (ord)
import Control.Monad.Reader

import Data.Sequence (Seq(..))
import Data.Set (Set)
import qualified Data.Sequence as Sq
import qualified Data.Set as S
import Data.Massiv.Array (Array, Ix2(..))
import qualified Data.Massiv.Array as A

-- data types
type Array2 a = A.Array A.U A.Ix2 a

data Area = Area
    { _heights :: Array2 Int
    , _start :: Ix2
    , _end :: Ix2} deriving (Eq, Ord, Show)


-- parsing
areaP :: Parser Area
areaP = do
    letterGrid <- A.fromLists' A.Seq <$> sepBy rowP newline
    return $ buildArea letterGrid


rowP :: Parser [Char]
rowP = many cellP


cellP :: Parser Char
cellP = lowerChar <|> char 'S' <|> char 'E'


buildArea :: Array2 Char -> Area
buildArea grid = Area (A.compute $ A.map ord' grid) (find grid 'S') (find grid 'E')


ord' :: Char -> Int
ord' 'S' = 0
ord' 'E' = 25
ord' c = ord c - 97


find :: Array2 Char -> Char -> Ix2
find grid target = A.ifoldlS f ((-1):.(-1)) grid
  where f :: Ix2 -> Ix2 -> Char -> Ix2
        f acc ix c
            | target == c       = ix
            | acc /= (-1):.(-1) = acc
            | otherwise         = acc


-- functions
shortestPath :: Area -> Int
shortestPath area = runReader pathFinder area


pathFinder :: MonadReader Area m => m Int 
pathFinder = do
    first <- reader _end
    go S.empty (Sq.singleton (first, 0))


go :: MonadReader Area m => Set Ix2 -> Seq (Ix2, Int) -> m Int
go seen Empty = error "Shouldn't have an empty queue"
go seen (x:<|xs) = do
    target <- reader _start
    case (target == fst x, S.member (fst x) seen) of
        (True, _)      -> return $ snd x
        (False, True)  -> go seen xs
        (False, False) -> do
            let curr = fst x
            let dist = snd x
            let nbs = neighbours curr
            grid <- reader _heights
            let height = A.index' grid curr
            -- make queue of those neighbours which are
            --  in bounds
            --  height <= current height + 1
            let nbs' = filter (f grid height) nbs
            let queue' = Sq.fromList (zip nbs' (repeat (dist+1)))
            go (S.insert (fst x) seen) (xs Sq.>< queue')


f :: Array2 Int -> Int -> Ix2 -> Bool
f grid height ix = (A.defaultIndex (-100) grid ix) >= (height - 1)


neighbours :: Ix2 -> [Ix2]
neighbours (i:.j) = [(i+1):.j, (i-1):.j, i:.(j+1), i:.(j-1)]


shortestPath' :: Area -> Int
shortestPath' area = runReader pathFinder' area


pathFinder' :: MonadReader Area m => m Int 
pathFinder' = do
    first <- reader _end
    go' S.empty (Sq.singleton (first, 0))



go' :: MonadReader Area m => Set Ix2 -> Seq (Ix2, Int) -> m Int
go' seen Empty = error "Shouldn't have an empty queue"
go' seen (x:<|xs) = do
    let curr = fst x
    grid <- reader _heights
    let height = A.index' grid curr
    case (height == 0, S.member (fst x) seen) of
        (True, _)      -> return $ snd x
        (False, True)  -> go' seen xs
        (False, False) -> do
            let dist = snd x
            let nbs = neighbours curr
            -- make queue of those neighbours which are
            --  in bounds
            --  height <= current height + 1
            let nbs' = filter (f grid height) nbs
            let queue' = Sq.fromList (zip nbs' (repeat (dist+1)))
            go' (S.insert (fst x) seen) (xs Sq.>< queue')



-- mains

mainA :: IO ()
mainA = do
    (Just area) <- parseInput areaP "12/input.txt"
    let answer = shortestPath area
    print answer
    -- result <- submitAnswer 2022 12 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just area) <- parseInput areaP "12/input.txt"
    let answer = shortestPath' area
    print answer
    -- result <- submitAnswer 2022 12 2 answer
    -- print result
    return ()
