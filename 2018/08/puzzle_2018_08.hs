module Puzzle_2018_08 where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import System.IO
import Automation (submitAnswer)

-- data types
type Parser = Parsec Void String

-- parsing
parseInput :: String -> IO [Int]
parseInput filename = do
    raw <- readFile filename
    let (Right tree) = runParser treeParser "" raw
    return tree

treeParser :: Parser [Int]
treeParser = (L.decimal) `sepBy` space

-- functions

metadataSum :: [Int] -> Int
metadataSum tree = helper [] tree where
    helper :: [(Int, Int)] -> [Int] -> Int
    helper _ [] = 0
    helper [] (x:y:xs) = helper [(x, y)] xs
    helper ((c, m):ss) xs@(x:xt)
        | (c, m) == (0, 0) = helper ss xs
        | c == 0           = x + helper ((c, m-1):ss) xt
        | c > 0            = helper ((x, head xt):(c-1, m):ss) (tail xt)


value :: [Int] -> (Int, [Int])
value xs@(x:y:xt)
    | x == 0    = (sum $ take y xt, drop y xt)
    | otherwise = (alignMetadata cvs (take y r), drop y r)
    where 
        (cvs, r) = childValues x xt


childValues :: Int -> [Int] -> ([Int], [Int])
childValues 0 tree = ([], tree)
childValues n tree = (v:vals, remainder) where
    (v, next) = value tree
    (vals, remainder) = childValues (n-1) next


alignMetadata :: [Int] -> [Int] -> Int
alignMetadata cvs [] = 0
alignMetadata cvs (m:ms)
    | 0 < m && m <= length cvs  = cvs !! (m-1) + alignMetadata cvs ms
    | otherwise                 = alignMetadata cvs ms

-- main functions

mainA :: IO ()
mainA = do
    tree <- parseInput "input.txt"
    let answer = metadataSum tree
    print answer
    -- result <- submitAnswer 2018 08 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    tree <- parseInput "input.txt"
    let (answer, _) = value tree
    print answer
    result <- submitAnswer 2018 08 2 answer
    print result
    return ()
