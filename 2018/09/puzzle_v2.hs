{-# LANGUAGE BangPatterns #-}
-- import Data.CircularList (CList)
-- import qualified Data.CircularList as CList
-- import Data.Maybe
import Control.Lens                     (ix, (+~))
import Data.Function                    ((&))
import Data.List                        (foldl')
import Data.Maybe                       (fromJust)
import Data.List.PointedList.Circular   (PointedList(..))
import Data.Vector                      (Vector)
import System.IO
import qualified Data.List.PointedList.Circular     as PL
import qualified Data.Vector                        as V
-- import Automation (submitAnswer)


-- learning by copying from https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-9
type Board = PointedList Int
type ScoreBoard = Vector Int

play :: Board -> Int -> (Int, Board)
play board marble
    | marble `mod` 23 == 0  = (marble + deleted, fromJust $ PL.deleteRight backSeven)
    | otherwise             = (0, PL.insertRight marble $ PL.moveN 1 board)
    where
        backSeven = PL.moveN (-7) board
        deleted = _focus backSeven


scores :: Int -> Int -> Vector Int
scores players final = fst . foldl' playHelper (scoreboard, start) $ zip turnOrder marbles 
    where
        playHelper :: (ScoreBoard, Board) -> (Int, Int) -> (ScoreBoard, Board)
        playHelper (!sb, !b) (!p, !m) = (sb & ix p +~ points, b') 
            where (points, b') = play b m
        scoreboard = V.replicate players 0
        start = PL.singleton 0
        marbles = [1..final]
        turnOrder = cycle [0..(players-1)]


highScore :: Int -> Int -> Int
highScore players final = V.maximum $ scores players final



-- mains
mainA :: IO ()
mainA = do
    let answer = highScore 464 71730
    print answer
    -- result <- submitAnswer 2018 09 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    let answer = highScore 464 7173000
    print answer
    -- result <- submitAnswer 2018 09 2 answer
    -- print result
    return ()


main :: IO ()
main = do
    mainA
    mainB
