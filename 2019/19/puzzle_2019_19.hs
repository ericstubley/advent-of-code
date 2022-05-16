module Puzzle_2019_19 where

import Automation (submitAnswer)
import Intcode
import Parsing
import Grid (printMap, printableMap)
import Utilities (minimumWith)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M


beamDisplay :: Int -> Char
beamDisplay i = case i of
    0 -> '.'
    1 -> '#'

scan :: Program -> Int -> Map (Int, Int) Int
scan program size = M.fromList (map beamer grid) where
    grid = [(x, y) | x <- [0..(size-1)], y <- [0..(size-1)]]
    beamer :: (Int, Int) -> ((Int, Int), Int)
    beamer (x, y) = ((x, y), head . snd $ runProgram program [x, y])


goodBeam :: Program -> (Int, Int) -> Bool
goodBeam program (x, y) = goodX program (x, y) && goodY program (x, y)


goodX :: Program -> (Int, Int) -> Bool
goodX program (x, y) = oo==1 && ox == 1 where
    oo = head . snd $ runProgram program [x, y]
    ox = head . snd $ runProgram program [x+99, y]


goodY :: Program -> (Int, Int) -> Bool
goodY program (x, y) = oo==1 && oy == 1 where
    oo = head . snd $ runProgram program [x, y]
    oy = head . snd $ runProgram program [x, y+99]


manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x, y) (a, b) = abs (x-a) + abs (y-b)


-- starting from a good point
backwardsSearch :: Program -> (Int, Int) -> (Int, Int)
backwardsSearch program (x, y)
    | (x0, y0) == (x, y) = (x, y)
    | otherwise          = backwardsSearch program (x0, y0)
    where
        xSearch a = goodBeam program (a, y)
        ySearch b = goodBeam program (x, b)
        x0 = binarySearch xSearch 0 x
        y0 = binarySearch ySearch 0 y


-- binarySearch lower is a known false, upper is a known true
-- find the smallest n with check n == True
binarySearch :: (Int -> Bool) -> Int -> Int -> Int
binarySearch check lower upper 
    | upper - lower < 2 = if check lower then lower else upper
    | check mid         = binarySearch check lower mid
    | otherwise         = binarySearch check mid upper
    where mid = div (upper + lower) 2


gridSearch :: Program -> Int -> (Int, Int) -> (Int, Int)
gridSearch program size (x, y) = minimumWith (manhattan (0, 0)) goodPoints 
  where grid = [(a, b) | a <- [(x-size+1)..x], b <- [(y-size+1)..y]]
        goodPoints = filter (goodBeam program) grid



mainA :: IO ()
mainA = do
    (Just tractor) <- parseInput programP "19/input.txt"
    let scanResults = scan tractor 50
    printMap $ printableMap beamDisplay 0 scanResults
    let answer = length . (filter (==1)) . M.elems $ scanResults
    print answer
    -- result <- submitAnswer 2019 19 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just tractor) <- parseInput programP "19/input.txt"
    -- 10000, 5000 is an observationally found good point
    let (bx, by) = backwardsSearch tractor (10000, 5000)
    print (bx, by)
    let (gx, gy) = gridSearch tractor 20 (bx, by)
    let answer = 10000*gx + gy
    print answer
    -- result <- submitAnswer 2019 19 2 answer
    -- print result
    return ()
