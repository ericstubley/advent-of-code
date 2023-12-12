module Puzzle_2023_11 where

import Automation (submitAnswer)
import Parsing
import Data.Massiv.Array (Array, Ix2(..), Sz(..))
import qualified Data.Massiv.Array as A
import Control.Monad.Reader

-- data types
type Array2 a = A.Array A.B A.Ix2 a
type Universe = Array2 Char

-- parsing
universeP :: Parser Universe
universeP = A.fromLists' A.Seq <$> sepBy (many astroP) newline

astroP :: Parser Char
astroP = char '.' <|> char '#'

-- functions
analyze :: Int -> Universe -> Int
analyze expansion universe = div (sum . map (inflateDistance expansion rows cols) $ pairs) 2
  where gs = runReader galaxies universe
        pairs = [(a, b) | a <- gs, b <- gs]
        rows = runReader weft universe
        cols = runReader warp universe

analyzeNow :: Universe -> Int
analyzeNow = analyze 2

analyzeLater :: Universe -> Int
analyzeLater = analyze 1000000

galaxies :: MonadReader Universe m => m [Ix2]
galaxies = do
    u <- reader id
    s <- reader $ A.size
    let (Sz (r:.c)) = s
    let ixs = [(x :. y) | x <- [0..(r-1)], y <- [0..(c-1)]]
    return $ filter (\ix -> (u A.! ix) == '#') ixs

-- rows but more numinous
weft :: MonadReader Universe m => m [Int]
weft = do
    u <- reader id
    s <- reader $ A.size
    let (Sz (r:.c)) = s
    return $ filter (\i -> A.all (=='.') (u A.!> i)) [0..(r-1)]

-- cols but more numinous
warp :: MonadReader Universe m => m [Int]
warp = do
    u <- reader id
    s <- reader $ A.size
    let (Sz (r:.c)) = s
    return $ filter (\j -> A.all (=='.') (u A.<! j)) [0..(c-1)]

inflateDistance :: Int -> [Int] -> [Int] -> (Ix2, Ix2) -> Int
inflateDistance expansion rows cols ((x1:.y1), (x2:.y2)) = dx + dy + (expansion - 1)*(ix + iy)
  where dx = abs (x1 - x2)
        dy = abs (y1 - y2)
        ix = length . takeWhile (< max x1 x2) . dropWhile (< min x1 x2) $ rows
        iy = length . takeWhile (< max y1 y2) . dropWhile (< min y1 y2) $ cols

-- mains

mainA :: IO ()
mainA = do
    (Just universe) <- parseInput universeP "11/input.txt"
    let answer = analyzeNow universe
    print answer
    -- result <- submitAnswer 2023 11 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just universe) <- parseInput universeP "11/input.txt"
    let answer = analyzeLater universe
    print answer
    -- result <- submitAnswer 2023 11 2 answer
    -- print result
    return ()
