module Puzzle_2018_06 where

import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import System.IO
import Automation (submitAnswer)
type Parser = Parsec Void String


-- data types
data Point = Point {x :: Int, y :: Int} deriving (Show, Eq, Ord)


-- parsing
parseInput :: String -> IO [Point]
parseInput filename = do
    raw <- readFile filename
    let (Right points) = runParser linesParser "" raw
    return points


linesParser :: Parser [Point]
linesParser = lineParser `sepBy` char '\n'


lineParser :: Parser Point
lineParser = do
   px <- L.decimal
   string ", "
   py <- L.decimal
   return $ Point px py


-- actual functions
manhattan :: Point -> Point -> Int
manhattan (Point ax ay) (Point bx by) = abs (ax - bx) + abs(ay - by)


findBoundary :: [Point] -> (Point, Point)
findBoundary ps = (Point minX minY, Point maxX maxY) where
    minX = minimum $ map x ps
    minY = minimum $ map y ps
    maxX = maximum $ map x ps
    maxY = maximum $ map y ps


allBoundary :: (Point, Point) -> [Point]
allBoundary (tl, br) = concat [ [Point (x tl) py | py <- [y tl .. y br]]
                              , [Point (x br) py | py <- [y tl .. y br]]
                              , [Point px (y tl) | px <- [(x tl) + 1 .. (x br) - 1]]
                              , [Point px (y br) | px <- [(x tl) + 1 .. (x br) - 1]]]


allGrid :: (Point, Point) -> [Point]
allGrid (tl, br) = [Point px py | px <- [x tl .. x br], py <- [y tl .. y br]]


interiorGrid :: (Point, Point) -> [Point]
interiorGrid (Point minX minY, Point maxX maxY) = allGrid (tl, br) where
    tl = Point (minX + 1) (minY + 1)
    br = Point (maxX - 1) (maxY - 1)
    

onBoundary :: (Point, Point) -> Point -> Bool
onBoundary (tl, br) p = (x p == x tl) || (x p == x br) || (y p == y tl) || (y p == y br)


closestPoint :: Point -> [Point] -> Maybe Point
closestPoint p ps
    | length ms > 1 = Nothing
    | otherwise     = Just (head ms)
    where
        ms = filter (\x -> manhattan p x == m) ps where
            m = minimum $ map (manhattan p) ps



findBoundaryPoints :: [Point] -> Set Point
findBoundaryPoints ps = Set.fromList $ filter (onBoundary (findBoundary ps)) ps


-- find the points which aren't closest to anything on the boundary
finiteAreaPoints :: [Point] -> Set Point
finiteAreaPoints ps = foldl updater (Set.fromList ps) (allBoundary $ findBoundary ps) where
    updater s bp 
        | cp == Nothing = s
        | otherwise     = Set.delete (fromJust cp) s
        where
            cp = closestPoint bp ps



interiorAreas :: [Point] -> Map Point Int
interiorAreas ps = foldl updater (Map.fromList $ zip ps (repeat 0)) (interiorGrid $ findBoundary ps) where
    updater m gp
        | cp == Nothing = m
        | otherwise     = Map.adjust (+1) (fromJust cp) m
        where
            cp = closestPoint gp ps


maxFiniteArea :: [Point] -> Int
maxFiniteArea ps = maximum $ map snd $ Map.toList $ Map.restrictKeys (interiorAreas ps) (finiteAreaPoints ps)


totalDistance :: Point -> [Point] -> Int
totalDistance p ps = sum $ map (manhattan p) ps


-- this confirmed that all boundary points are unsafe, so also all exterior points are
bufferTest :: [Point] -> Int
bufferTest ps = minimum $ map (\p -> totalDistance p ps) (allBoundary $ findBoundary ps)


safePoints :: Int -> [Point] -> [Point]
safePoints cutoff ps = filter (\p -> (totalDistance p ps) < cutoff) grid where
    grid = allGrid $ findBoundary ps





mainA :: IO ()
mainA = do
    points <- parseInput "input.txt"
    let answer = maxFiniteArea points
    print answer
    -- result <- submitAnswer 2018 06 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    points <- parseInput "input.txt"
    let answer = length $ safePoints 10000 points
    print answer
    result <- submitAnswer 2018 06 2 answer
    print result
    return ()
