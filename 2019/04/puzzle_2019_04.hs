module Puzzle_2019_04 where

import Automation (submitAnswer)


validity :: Int -> Bool
validity n = sixDigits ds && nonDecreasingDigits ds && repeatedDigit ds
    where ds = digits n

validity' :: Int -> Bool
validity' n = sixDigits ds && nonDecreasingDigits ds && repeatedDigit' ds
    where ds = digits n

digits :: Int -> [Int]
digits n = go n []
  where
    go :: Int -> [Int] -> [Int]
    go 0 ls = ls
    go n ls = go (div n 10) (mod n 10 : ls)


sixDigits :: [Int] -> Bool
sixDigits ds = length ds == 6


nonDecreasingDigits :: [Int] -> Bool
nonDecreasingDigits (x:xs@(y:_)) = (x <= y) && nonDecreasingDigits xs
nonDecreasingDigits _ = True


repeatedDigit :: [Int] -> Bool
repeatedDigit (x:xs@(y:_)) = (x == y) || repeatedDigit xs
repeatedDigit _ = False


repeatedDigit' :: [Int] -> Bool
repeatedDigit' (a:b:c:d:e:f:[]) = or [t1, t2, t3, t4, t5]
  where
    t1 = a == b && b < c
    t2 = a < b && b == c && c < d
    t3 = b < c && c == d && d < e
    t4 = c < d && d == e && e < f
    t5 = d < e && e == f


validInRange :: Int -> Int -> [Int]
validInRange lower upper = filter validity [lower..upper]

validInRange' :: Int -> Int -> [Int]
validInRange' lower upper = filter validity' [lower..upper]




mainA :: IO ()
mainA = do
    let answer = length $ validInRange 271973 785961 
    print answer
    -- result <- submitAnswer 2019 04 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    let answer = length $ validInRange' 271973 785961 
    print answer
    result <- submitAnswer 2019 04 2 answer
    print result
    return ()
