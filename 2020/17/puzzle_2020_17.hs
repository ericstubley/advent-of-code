module Puzzle_2020_17 where

import Automation (submitAnswer)
import Parsing
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Linear.V3
import Linear.V4
import Data.List (nub)

-- rather than figuring out the polymorphism, just duplicate code for 4d version

-- data types
type Grid a = Map a Bool
type CountGrid a = Map a Int
type Grid3 = Grid (V3 Int)
type Grid4 = Grid (V4 Int)
type CountGrid3 = CountGrid (V3 Int)
type CountGrid4 = CountGrid (V4 Int)

-- parsing
gridP :: Parser Grid3
gridP = gridFromList <$> sepBy rowP newline

grid4P :: Parser Grid4
grid4P = grid4FromList <$> sepBy rowP newline

rowP :: Parser [Bool]
rowP = some cellP

cellP :: Parser Bool
cellP = (char '#' >> return True) <|> (char '.' >> return False)


gridFromList :: [[Bool]] -> Grid3
gridFromList rows = M.filter id $ M.fromList prepared
  where yBound = length rows
        xBound = length $ head rows
        prepared = [(V3 x y 0, (rows !! y) !! x) 
                    | x <- [0..(xBound-1)]
                    , y <- [0..(yBound-1)]]


grid4FromList :: [[Bool]] -> Grid4
grid4FromList rows = M.filter id $ M.fromList prepared
  where yBound = length rows
        xBound = length $ head rows
        prepared = [(V4 x y 0 0, (rows !! y) !! x) 
                    | x <- [0..(xBound-1)]
                    , y <- [0..(yBound-1)]]


-- functions
neighbours :: V3 Int -> [V3 Int]
neighbours v = [v + (V3 dx dy dz) 
                | dx <- [-1, 0, 1]
                , dy <- [-1, 0, 1]
                , dz <- [-1, 0, 1]
                , (dx, dy, dz) /= (0, 0, 0)]


neighbours4 :: V4 Int -> [V4 Int]
neighbours4 v = [v + (V4 dx dy dz dw) 
                | dx <- [-1, 0, 1]
                , dy <- [-1, 0, 1]
                , dz <- [-1, 0, 1]
                , dw <- [-1, 0, 1]
                , (dx, dy, dz, dw) /= (0, 0, 0, 0)]



-- assuming grid is just the ones where they're alive
neighbourCounts :: Grid3 -> CountGrid3
neighbourCounts grid = M.unionsWith (+) neighbourhoods
  where neighbourhoods = map (\k -> M.fromList $ zip (neighbours k) (repeat 1)) (M.keys grid)


neighbourCounts4 :: Grid4 -> CountGrid4
neighbourCounts4 grid = M.unionsWith (+) neighbourhoods
  where neighbourhoods = map (\k -> M.fromList $ zip (neighbours4 k) (repeat 1)) (M.keys grid)


-- given the current cell state and number of neighbours, find new state
updateCell :: Bool -> Int -> Bool
updateCell True friends = friends == 2 || friends == 3
-- if you're alive you stay alive when you have either 2 or 3 friends
updateCell False friends = friends == 3
-- if you're dead you become alive when you have exactly 3 friends


-- cells that might get updated
-- those with a non-zero neighbourCount
-- those in the original grid
step :: Grid3 -> Grid3
step grid = M.filter id $ updated
  where ncs = neighbourCounts grid
        possible = nub $ (M.keys grid) ++ (M.keys ncs)
        updated = M.fromList $ map (\v -> (v, state v)) possible
        state v = updateCell (M.findWithDefault False v grid) (M.findWithDefault 0 v ncs)


step4 :: Grid4 -> Grid4
step4 grid = M.filter id $ updated
  where ncs = neighbourCounts4 grid
        possible = nub $ (M.keys grid) ++ (M.keys ncs)
        updated = M.fromList $ map (\v -> (v, state v)) possible
        state v = updateCell (M.findWithDefault False v grid) (M.findWithDefault 0 v ncs)


numberActive :: Map a Bool -> Int
numberActive grid = length $ filter id (M.elems grid)

-- mains

mainA :: IO ()
mainA = do
    (Just grid) <- parseInput gridP "17/input.txt"
    let answer = numberActive $ (iterate step grid) !! 6
    print answer
    -- result <- submitAnswer 2020 17 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just grid) <- parseInput grid4P "17/input.txt"
    let answer = numberActive $ (iterate step4 grid) !! 6
    print answer
    -- result <- submitAnswer 2020 17 2 answer
    -- print result
    return ()
