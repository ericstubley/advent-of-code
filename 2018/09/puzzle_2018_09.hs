module Puzzle_2018_09 where

import Data.CircularList (CList)
import qualified Data.CircularList as CList
import Data.Maybe
import System.IO
import Automation (submitAnswer)


-- functions
highScore :: Int -> Int -> Int
highScore players final = maximum $ playGame players final

playGame :: Int -> Int -> [Int]
playGame players final = summedScores 1 where
    summedScores :: Int -> [Int]
    summedScores p
        | p == players + 1  = []
        | otherwise         = (sum $ map snd $ filter (\x -> fst x == p) ss) : summedScores (p+1)
        where
            ss = filter (\x -> snd x /= 0) $ take (final+1) $ scores players

-- infinite list of who plays and what they score on each turn
scores :: Int -> [(Int, Int)]
scores players = zip (cycle [1..players]) boardScores

-- infinite list of scores on each turn
boardScores :: [Int]
boardScores = (0 : map nextBoardScore boards)

nextBoardScore :: CList Int -> Int
nextBoardScore board
    | mod f 23 == 22   = 1 + f + fromJust (CList.focus $ CList.rotN (-7) board)
    | otherwise        = 0
    where
        f = fromJust $ CList.focus board


-- infinite list of board states
boards :: [CList Int]
boards = scanl playMarble (CList.fromList [0]) [1..]


playMarble :: CList Int -> Int -> CList Int
playMarble board marble
    | mod marble 23 == 0    = CList.removeR $ CList.rotN (-7) board 
    | otherwise             = CList.insertL marble $ CList.rotR board


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
