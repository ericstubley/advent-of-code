module Puzzle_2018_14 where

import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import System.IO
import Automation (submitAnswer)


-- data types
data Scoreboard = Scoreboard {elf1 :: Int, elf2 :: Int, board :: Seq Int} deriving (Eq, Show)


-- functions

startingBoard :: Scoreboard
startingBoard = Scoreboard 0 1 (S.fromList [3, 7])


tenAfter :: Int -> [Int]
tenAfter n = helper n startingBoard where
    helper :: Int -> Scoreboard -> [Int]
    helper n sb
        | S.length b < n + 10   = helper n (newRecipe sb)
        | otherwise             = toList . S.take 10 . S.drop n $ b
        where b = board sb


newRecipe :: Scoreboard -> Scoreboard
newRecipe (Scoreboard e1 e2 sb) = Scoreboard e1' e2' sb' where
    s1 = S.index sb e1
    s2 = S.index sb e2
    sb' | s1 + s2 < 10  = sb |> (s1+s2)
        | otherwise     = sb |> 1 |> mod (s1+s2) 10
    len = S.length sb'
    e1' = mod (e1 + s1 + 1) len
    e2' = mod (e2 + s2 + 1) len


recipesLeft :: [Int] -> Int
recipesLeft scores = helper (S.fromList scores) startingBoard where
    helper :: Seq Int -> Scoreboard -> Int
    helper ts sb
        | t1 == ts  = bl - tl
        | t2 == ts  = bl - tl - 1
        | otherwise = helper ts (newRecipe sb)
        where 
            b = board sb
            tl = S.length ts
            bl = S.length b
            t1 = S.drop (bl - tl) b
            t2 = S.take tl . S.drop (bl - tl - 1) $ b







mainA :: IO ()
mainA = do
    let ls = tenAfter 260321 
    let answer = concat $ map show ls 
    print answer
    -- result <- submitAnswer 2018 14 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    let answer = recipesLeft [2,6,0,3,2,1]
    print answer
    result <- submitAnswer 2018 14 2 answer
    print result
    return ()
