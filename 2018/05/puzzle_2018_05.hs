module Puzzle_2018_05 where

import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (nub)
import Data.List.Extra (minimumOn)
import System.IO
import Automation (submitAnswer)


parseInput :: String -> IO String
parseInput filename = do
    polymer <- readFile filename
    return polymer


-- found this online: https://stackoverflow.com/questions/7442892/
-- look through an (infinite) list until you find two adjacent elements
-- which match a predicate
converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y     = y
    | otherwise = converge p ys


react :: String -> String
react [] = []
react [x] = [x]
react (x:xs@(y:ys))
    | switchCase x == y = react ys
    | otherwise         = x : react xs


switchCase :: Char -> Char
switchCase c
    | isUpper c = toLower c
    | isLower c = toUpper c
    | otherwise = c

-- original version based on things above
-- stabilize :: String -> String
-- stabilize s = converge (==) (iterate (react) s)


halve :: [a] -> ([a], [a])
halve xs = ((take s xs), (drop s xs))
    where s = div (length xs) 2


merge :: String -> String -> String
merge [] ys = ys
merge xs [] = reverse xs
merge xs@(x:xss) ys@(y:yss)
    | switchCase x == y = merge xss yss
    | otherwise         = (reverse xs) ++ ys

-- my attempt at the divide and conquer strat
-- stabilize :: String -> String
-- stabilize [] = []
-- stabilize [x] = [x]
-- stabilize s = merge (reverse $ stabilize l) (stabilize r) where
--     (l, r) = halve s


-- even simpler solution from reddit
-- remember to use foldr
-- you didn't want to use foldl because you'd need to access the last of the acc every time
-- things are probably optimized for foldr so this'll still be fast
stabilize :: String -> String
stabilize s = foldr step "" s where
    step x (y:ys)
        | switchCase x == y = ys
    step x ys = x:ys

-- you could just as easily reverse first and think of having two stacks; one you pop off of one onto




removeUnit :: Char -> String -> String
removeUnit c s = filter (\x -> (x /= c) && (x /= toUpper c)) s


shortestReaction :: String -> String
shortestReaction s = minimumOn length (map stabilize (shortenedPolymers s))
    

shortenedPolymers :: String -> [String]
shortenedPolymers s = map (\c -> removeUnit c s) (units s)


units :: String -> String
units s = nub $ map toLower s



mainA :: IO ()
mainA = do
    polymer <- parseInput "input.txt"
    let stablePolymer = stabilize polymer
    let answer = length stablePolymer
    print answer
    -- result <- submitAnswer 2018 05 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    polymer <- parseInput "input.txt"
    let stablestPolymer = shortestReaction polymer
    let answer = length stablestPolymer
    print answer
    -- result <- submitAnswer 2018 05 2 answer
    -- print result
    return ()
