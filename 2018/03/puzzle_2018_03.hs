module Puzzle_2018_03 where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import System.IO
import Automation (submitAnswer)

import Text.Megaparsec
type Parser = Parsec Void String

-- data types
data Point = Point {x::Int, y::Int} deriving (Show, Eq, Ord)
data Rectangle = Rectangle {p::Point, width::Int, height::Int}
data LazyCount = Once | Lots deriving (Eq)

countUp :: LazyCount -> LazyCount -> LazyCount
countUp _ _ = Lots

-- parsing
parseInput :: String -> IO [(Int, Rectangle)]
parseInput filename = do
    raw <- readFile filename
    let (Right rects) = runParser linesParser "Some text" raw
    return rects


linesParser :: Parser [(Int, Rectangle)]
linesParser = lineParser `sepBy` char '\n'


lineParser :: Parser (Int, Rectangle)
lineParser = do
   char '#'
   ident <- L.decimal 
   string " @ "
   x <- L.decimal
   char ','
   y <- L.decimal
   string ": "
   width <- L.decimal
   char 'x'
   height <- L.decimal
   return (ident, Rectangle (Point x y) width height)





-- counts on grid
totalOverlap :: [Rectangle] -> Int
totalOverlap rects = length $ Map.filter (==Lots) built where
    built = foldl addRectangle Map.empty rects


addRectangle :: Map Point LazyCount -> Rectangle -> Map Point LazyCount
addRectangle m r = Map.unionWith countUp m (rectangleMap r)


rectangleMap :: Rectangle -> Map Point LazyCount
rectangleMap r = Map.fromList $ zip (rectanglePoints r) (cycle [Once])


rectanglePoints :: Rectangle -> [Point]
rectanglePoints r = [Point px py | px <- xRange, py <- yRange] where
    xRange = [xStart..xStart + (width r) - 1]
    xStart = x (p r)
    yRange = [yStart..yStart + (height r) - 1]
    yStart = y (p r)


allOnces :: Map Point LazyCount -> Rectangle -> Bool
allOnces m r = all (==Once) (map ((Map.!) m) (rectanglePoints r))


extractID :: [(Int, Rectangle)] -> Int
extractID rects = fst . head $ filter isCorrect rects where
    isCorrect (i, r) = allOnces built r
    built = foldl addRectangle Map.empty (map snd rects)



mainA :: IO ()
mainA = do
    rects <- parseInput "input.txt"
    let justRects = map snd rects
    let answer = totalOverlap justRects
    print answer
    -- result <- submitAnswer 2018 03 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    rects <- parseInput "input.txt"
    let answer = extractID rects
    print answer
    -- result <- submitAnswer 2018 03 2 answer
    -- print result
    return ()
