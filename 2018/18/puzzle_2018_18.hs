module Puzzle_2018_18 where


import Data.List (findIndices)
import Data.Massiv.Array (Array, Ix2(..))
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array.Stencil as A.Stencil
import System.IO
import Automation (submitAnswer)

-- data types
type Array2 a = A.Array A.U A.Ix2 a


-- parsing
parseInput :: String -> IO (Array2 Char)
parseInput filename = do
    raw <- readFile filename
    let ls = lines raw
    let a = A.fromLists' A.Seq ls
    return a


-- functions

center :: Ix2
center = 0 :. 0


neighbours :: [Ix2]
neighbours = [-1 :. -1, -1 :. 0, -1 :. 1
             , 0 :. -1,           0 :. 1
             , 1 :. -1,  1 :. 0,  1 :. 1]  


count :: (Eq a) => a -> [a] -> Int
count a as = sum $ map (\x -> if x==a then 1 else 0) as


countArr :: (A.Unbox a, Eq a) => a -> Array2 a -> Int
countArr a arr = A.sum $ A.map (\x -> if x==a then 1 else 0) arr            


grove :: A.Stencil Ix2 Char Char
grove = A.makeStencil (A.Sz (3 :. 3)) (1 :. 1) quanta where
    quanta get
        | get center == '.' = if count '|' nbhd >= 3
                                    then '|'
                                    else '.'
        | get center == '|' = if count '#' nbhd >= 3
                                    then '#'
                                    else '|'
        | get center == '#' = if (count '|' nbhd >= 1) && (count '#' nbhd >= 1)
                                    then '#'
                                    else '.'
        where nbhd = map get neighbours


magic :: Array2 Char -> Array2 Char
magic a = A.compute $ A.mapStencil (A.Fill 'u') grove a 


eons :: Int -> Array2 Char -> Array2 Char
eons 0 arr = arr
eons n arr = eons (n-1) (magic arr)


period :: [Array2 Char] -> [Int]
period arrs
    | head arrs `elem` tail arrs    = findIndices (== (head arrs)) (reverse arrs)
    | otherwise                     = period $ (magic $ head arrs) : arrs


longtermValue :: Int -> Array2 Char -> Int
longtermValue n arr = value $ eons t arr where
    (t1:t2:_) = period [arr]
    modulus = t2 - t1
    remainder = mod (n-t1) modulus
    t = t1 + remainder


value :: Array2 Char -> Int
value arr = (countArr '|' arr) * (countArr '#' arr)


-- mains

mainA :: IO ()
mainA = do
    forest <- parseInput "input.txt"
    let forest10 = eons 10 forest
    let answer = value forest10
    print answer
    -- result <- submitAnswer 2018 18 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    forest <- parseInput "input.txt"
    let answer = longtermValue 1000000000 forest
    print answer
    result <- submitAnswer 2018 18 2 answer
    print result
    return ()
