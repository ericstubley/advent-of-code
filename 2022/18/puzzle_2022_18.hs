module Puzzle_2022_18 where

import Automation (submitAnswer)
import Parsing
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- data types
data Tile = Exterior | Lava deriving (Eq, Ord, Show)

type Cube = (Int, Int, Int)

-- parsing
cubesP :: Parser [Cube]
cubesP = sepBy cubeP newline


cubeP :: Parser Cube
cubeP = do
   x <- natural
   y <- (char ',') >> natural
   z <- (char ',') >> natural
   return (x, y, z)


-- functions
perturbations :: [Cube]
perturbations = [ (1, 0, 0), (-1, 0, 0)
                , (0, 1, 0), (0, -1, 0)
                , (0, 0, 1), (0, 0, -1)]


allThree :: (a -> b -> c) -> (a, a, a) -> (b, b, b) -> (c, c, c)
allThree f (x1, x2, x3) (y1, y2, y3) = (f x1 y1, f x2 y2, f x3 y3)


perturb :: Cube -> [Cube]
perturb cube = map (allThree (+) cube) perturbations


surfaceArea :: [Cube] -> Int
surfaceArea cubes = sum $ map countFaces cubes where
    cubeSet = S.fromList cubes
    countFaces :: Cube -> Int
    countFaces cube = length . (filter (\c -> S.notMember c cubeSet)) $ perturb cube


exteriorSurfaceArea :: [Cube] -> Int
exteriorSurfaceArea cubes = (surfaceArea cubes) - (interiorSurfaceArea cubes)


interiorSurfaceArea :: [Cube] -> Int
interiorSurfaceArea cubes = sum $ map countInteriorFaces cubes where
    cubeMap = buildMap cubes
    countInteriorFaces :: Cube -> Int
    countInteriorFaces cube = length . (filter (\c -> M.notMember c cubeMap)) $ perturb cube


buildMap :: [Cube] -> Map Cube Tile
buildMap cubes = go (M.fromList $ zip cubes (repeat Lava)) [(-1,-1,-1)] where
    upper = (findMax cubes) + 1
    go :: Map Cube Tile -> [Cube] -> Map Cube Tile
    go m [] = m
    go m (q:qs)
        | M.notMember q m = go m' qs'
        | otherwise       = go m qs
          where m' = M.insert q Exterior  m
                explore = filter (inBounds (-1) upper) (perturb q)
                qs' = qs ++ explore


inBounds :: Int -> Int -> Cube -> Bool
inBounds lower upper (x, y, z) = bx && by && bz where
    bx = (lower <= x) && (x <= upper)
    by = (lower <= y) && (y <= upper)
    bz = (lower <= z) && (z <= upper)


findMax :: [Cube] -> Int
findMax cubes = foldl f 0 cubes where
    f :: Int -> Cube -> Int
    f acc (a, b, c) = maximum [acc, a, b, c]

 -- mains

mainA :: IO ()
mainA = do
    (Just cubes) <- parseInput cubesP "18/input.txt"
    let answer = surfaceArea cubes
    print answer
    -- result <- submitAnswer 2022 18 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just cubes) <- parseInput cubesP "18/input.txt"
    let answer = exteriorSurfaceArea cubes
    print answer
    -- result <- submitAnswer 2022 18 2 answer
    -- print result
    return ()
