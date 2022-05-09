module Puzzle_2019_10 where

import Automation (submitAnswer)
import Grid
import Parsing
import Utilities (maximumWith, minWith)
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import Data.Sort (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Massiv.Array as A

-- data types
type AsteroidField = Map Ix2 (Map Ix2 Ix2)

-- parsing
asteroidFieldP :: Parser AsteroidField
asteroidFieldP = do
    grid <- asteroidGridP
    return $ lineOfSightMap grid


asteroidGridP :: Parser (Array2 Char)
asteroidGridP = do
    lls <- sepBy (many asteroidP) newline
    return (A.fromLists' A.Seq lls)


asteroidP :: Parser Char
asteroidP = (char '.') <|> (char '#')


-- functions

-- this is the meat of part a; take a grid and turn it into a graph of
-- asteroid -> direction -> closest asteroid in the direction
-- basically a foldl over an Array2
-- whenever we see a new asteroid, add an entry for it and then
-- compare over all entries of the map; whenever our new one is the closest
-- in its direction, add it in both places
lineOfSightMap :: Array2 Char -> AsteroidField
lineOfSightMap grid = A.ifoldlS addAsteroid (M.empty) grid



addAsteroid :: AsteroidField -> Ix2 -> Char -> AsteroidField
addAsteroid af _ '.' = af
addAsteroid af ix _  = M.foldlWithKey isVisible af' af where
    af' = M.insert ix M.empty af
    isVisible :: AsteroidField -> Ix2 -> Map Ix2 Ix2 -> AsteroidField
    isVisible acc vis dirs
        | not (M.member dir dirs) = acc'
        | closer = acc'
        | otherwise = acc
        where
            dir = direction vis ix
            fdir = flipDir dir
            competitor = dirs M.! dir
            closer = (distance vis ix) < (distance vis competitor)
            acc' = M.insert ix  (M.insert fdir vis (acc M.! ix)) $
                   M.insert vis (M.insert dir ix dirs) acc



numberVisible :: AsteroidField -> Ix2 -> Int
numberVisible af a = M.size $ M.findWithDefault (M.empty) a af


bestAsteroid :: AsteroidField -> Ix2
bestAsteroid af = maximumWith (numberVisible af) (M.keys af)


direction :: Ix2 -> Ix2 -> Ix2
direction (y1:.x1) (y2:.x2) = (y:.x) where
    y' = y2 - y1
    x' = x2 - x1
    g = gcd y' x'
    y = div y' g
    x = div x' g


flipDir :: Ix2 -> Ix2
flipDir (y:.x) = (-y):.(-x)


distance :: Ix2 -> Ix2 -> Int
distance (y1:.x1) (y2:.x2) = abs (y1-y2) + abs (x1-x2)


-- part b is a kind of different map
-- strategy is to put all the asteroids into a Map Float [Ix2]; the float is
-- the angle in [0, 2*pi) between up and the direction vector from base to 
-- the asteroid. The desctruction order is then just the heads of these lists
destructionOrder :: AsteroidField -> Ix2 -> [Ix2]
destructionOrder af base = go $ angleMap af base where
    go :: Map Float [Ix2] -> [Ix2]
    go targets
        | M.null targets = []
        | otherwise      = heads ++ go targets'
        where 
            heads = map head $ M.elems targets
            targets' = M.filter (not . null) $ M.map tail targets


angleMap :: AsteroidField -> Ix2 -> Map Float [Ix2]
angleMap af base = M.map (sortOn (distance base)) tempMap
  where
    targets = filter (/= base) (M.keys af)
    tempMap = foldl inserter M.empty targets
    inserter :: Map Float [Ix2] -> Ix2 -> Map Float [Ix2]
    inserter acc target = M.unionWith (++) acc (M.singleton theta [target])
        where theta = angleFromUp (direction base target)


-- dot product against (-1:.0)
-- positive x are 0 to pi, negative x are pi to 2*pi
angleFromUp :: Ix2 -> Float
angleFromUp (dy:.dx)
    | dx >= 0 = arccos
    | dx <  0 = 2*pi - arccos
    where arccos = acos $ (-dy') / (magnitude (dy:.dx))
          dy' = fromIntegral dy


magnitude :: Ix2 -> Float
magnitude (y:.x) = sqrt (y'**2 + x'**2) where
    y' = fromIntegral y
    x' = fromIntegral x



-- mains

mainA :: IO ()
mainA = do
    (Just af) <- parseInput asteroidFieldP "10/input.txt"
    let answer = numberVisible af (bestAsteroid af)
    print answer
    -- result <- submitAnswer 2019 10 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just af) <- parseInput asteroidFieldP "10/input.txt"
    let best = bestAsteroid af
    let order = destructionOrder af best
    let (y:.x) = order !! 199 -- want the 200th element of this list
    let answer = 100*x + y
    print answer
    -- result <- submitAnswer 2019 10 2 answer
    -- print result
    return ()
