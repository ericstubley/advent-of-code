{-# LANGUAGE BangPatterns #-}
module Puzzle_2018_11 where

import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.IO
import Automation (submitAnswer)

-- data types
type Point = (Int, Int)
type Region = ((Int, Int), Int)


powerLevel :: Int -> (Int, Int) -> Int
powerLevel serial (x, y) = (hundredsDigit (rackID * ((rackID * y) + serial))) - 5 where
    rackID = x + 10


hundredsDigit :: Int -> Int
hundredsDigit n = mod (div n 100) 10


regionSum :: Int -> Int -> (Int, Int) -> Int
regionSum serial regionDim (x, y) = sum $ map (powerLevel serial) region where
    region = [(x + dx, y + dy) | dx <- ls, dy <- ls]
    ls = [0..(regionDim-1)]


bestRegion :: Int -> Int -> (Int, (Int, Int))
bestRegion serial dimension = maximum $ map (\t -> (regionSum serial 3 t, t)) coords where
    coords = [(x, y) | x <- ls, y <- ls]
    ls = [1..dimension]


grid :: Int -> Int -> Map Point Int
grid serial dimension = M.fromList $ map (\t -> (t, pls t)) coords where
    pls = powerLevel serial
    coords = [(x, y) | x <- ls, y <- ls]
    ls = [1..dimension]


-- remove edge cases
-- add boundaries
gridN :: Int -> Int -> Int -> Map Point Int -> Map Point Int
gridN serial dim regionDim lower = M.mapWithKey boundaryAdder edgesRemoved where
    b = boundary serial regionDim
    edgeDetector :: Point -> Int -> Bool
    edgeDetector (x, y) _
        | x == (dim - regionDim + 1) || y == (dim - regionDim + 1)  = True
        | otherwise                                                 = False
    boundaryAdder :: Point -> Int -> Int
    boundaryAdder k v = (b k) + v
    edgesRemoved = M.filterWithKey (\k v -> not $ edgeDetector k v) lower


boundary :: Int -> Int -> (Int, Int) -> Int
boundary serial regionDim (x, y) = bot + right - br where
    offset  = regionDim-1
    pls     = powerLevel serial
    bot     = sum $ map pls [(x + dx, y + offset) | dx <- [0..offset]]
    right   = sum $ map pls [(x + offset, y + dy) | dy <- [0..offset]]
    br      = pls (x + offset, y + offset)


maxFromGrid :: Map Point Int -> (Int, Point)
maxFromGrid g = maximum $ map swap $ M.toList g


swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)


addRegion :: Int -> (Int, Point) -> (Int, Region)
addRegion region (v, p) = (v, (p, region))



bestRegionVariable :: Int -> Int -> (Int, Region)
bestRegionVariable serial dim = helper serial dim 1 M.empty where
    g = grid serial dim
    helper :: Int -> Int -> Int -> Map Point Int -> (Int, Region)
    helper (!serial) (!dim) (!regionDim) (!lower)
        | regionDim == 1    = max (ret g) (helper serial dim 2 g)
        | regionDim == dim  = ret gn
        | otherwise         = max (ret gn) (helper serial dim (regionDim+1) gn)
        where
            gn = gridN serial dim regionDim lower
            ret = (addRegion regionDim) . maxFromGrid


-- gridWithRegions :: Int -> Int -> Map ((Int, Int), Int) Int
-- gridWithRegions serial dimension = builder serial dimension dimension where
--     builder serial dim regionDim
--         | regionDim == 1    = M.mapKeys (\x -> (x, 1)) (grid serial dim)
--         | regionDim == 2    = M.union (M.fromList $ map (\t -> ((t, regionDim), smartRegion2 t lower)) coords) lower
--         | otherwise         = M.union (M.fromList $ map (\t -> ((t, regionDim), smartRegion regionDim t lower)) coords) lower
--         where
--             lower = builder serial dim (regionDim - 1)
--             coords = [(x, y) | x <- ls, y <- ls]
--             ls = [1..(dim - regionDim + 1)]
-- 
-- 
-- smartRegion2 :: Int -> (Int, Int) -> Int
-- smartRegion2 (x, y) (!computed) = r + rr + rd + rdr where
--     r   = computed M.! ((x, y), 1)
--     rr  = computed M.! ((x+1, y), 1)
--     rd  = computed M.! ((x, y+1), 1)
--     rdr = computed M.! ((x+1, y+1), 1)



mainA :: IO ()
mainA = do
    let (value, (bestX, bestY)) = bestRegion 1309 300
    let answer = (show bestX) ++ "," ++ (show bestY)
    print answer
    -- result <- submitAnswer 2018 11 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    let (value, ((bestX, bestY), regionDim)) = bestRegionVariable 1309 300
    let answer = (show bestX) ++ "," ++ (show bestY) ++ "," ++ (show regionDim)
    print answer
    result <- submitAnswer 2018 11 2 answer
    print result
    return ()
