module Puzzle_2023_10 where

import Automation (submitAnswer)
import Parsing
import Data.Massiv.Array (Array, Ix2(..))
import qualified Data.Massiv.Array as A
import Control.Monad.Reader
import Grid (Direction(..))
import Data.Maybe (fromJust)
import Control.Monad (filterM)

-- data types
type Array2 a = A.Array A.B A.Ix2 a

data Tile = NE | NS | NW | ES | EW | SW | Ground | Animal deriving (Eq, Show, Ord)

data Env = Env {_tiles :: Array2 Tile}

-- parsing
tilesP :: Parser Env 
tilesP = (Env . A.fromLists' A.Seq) <$> sepBy (many tileP) newline

tileP :: Parser Tile
tileP = (char 'L' >> return NE)
    <|> (char '|' >> return NS)
    <|> (char 'J' >> return NW)
    <|> (char 'F' >> return ES)
    <|> (char '-' >> return EW)
    <|> (char '7' >> return SW)
    <|> (char '.' >> return Ground)
    <|> (char 'S' >> return Animal)

-- functions
furthest :: Env -> Int
furthest env = div (length $ runReader loop env) 2

loop :: MonadReader Env m => m [Ix2]
loop = do
    start <- locate
    first <- embark start
    follow [first, start]

follow :: MonadReader Env m => [Ix2] -> m [Ix2]
follow path = do
    tile <- readTile (head path)
    if tile == Animal
        then return path
        else do
            next <- step path
            follow (next : path)

-- find the next step given the path
step :: MonadReader Env m => [Ix2] -> m Ix2
step path = do
    (a, b) <- steps (head path)
    if path !! 1 == a
        then return b
        else return a

steps :: MonadReader Env m => Ix2 -> m (Ix2, Ix2)
steps ix = do
    tile <- readTile ix
    return $ piping tile ix


piping :: Tile -> Ix2 -> (Ix2, Ix2)
piping NE (i :. j) = ((i-1) :. j, i :. (j+1))
piping NS (i :. j) = ((i-1) :. j, (i+1) :. j)
piping NW (i :. j) = ((i-1) :. j, i :. (j-1))
piping ES (i :. j) = (i :. (j+1), (i+1) :. j)
piping EW (i :. j) = (i :. (j+1), i :. (j-1))
piping SW (i :. j) = ((i+1) :. j, i :. (j-1))
piping Ground ix = (ix, ix)

-- find the starting location
locate :: MonadReader Env m => m Ix2
locate = (fromJust . A.findIndex (==Animal)) <$> reader _tiles

-- pick a starting direction given the starting location
embark :: MonadReader Env m => Ix2 -> m Ix2
embark ix = do
    sz <- reader (A.size . _tiles)
    candidates <- filterM candidate (filter (A.isSafeIndex sz) $ neighbours ix)
    return $ head candidates

-- given an index, return true when the Animal is in one of the neighbouring tiles
candidate :: MonadReader Env m => Ix2 -> m Bool
candidate ix = do
    tile <- readTile ix
    let (a, b) = piping tile ix
    start <- locate
    return $ start == a || start == b


readTile :: MonadReader Env m => Ix2 -> m Tile
readTile ix = reader $ \env -> A.index' (_tiles env) ix


neighbours :: Ix2 -> [Ix2]
neighbours (i :. j) = [(i-1) :. j, i :. (j+1), (i+1) :. j, i :. (j-1)]

printer :: Tile -> Char
printer NE = 'L'
printer NS = '|'
printer NW = 'J'
printer ES = 'F'
printer EW = '-'
printer SW = '7'
printer Ground = '.'
printer Animal = 'S'


enclosed :: Env -> Int
enclosed env = div (4*a - 2*l + 4) 4
  where path = loop env
        a = div (abs . shoelace $ path) 2
        l = length . tail $ path


shoelace :: [Ix2] -> Int
shoelace [] = 0
shoelace (x:[]) = 0
shoelace (x:xs@(y:ys)) = (a1*b2 - a2*b1) + shoelace xs
  where (a1 :. b1) = x
        (a2 :. b2) = y

-- mains

mainA :: IO ()
mainA = do
    (Just env) <- parseInput tilesP "10/input.txt"
    let answer = furthest env
    print answer
    -- result <- submitAnswer 2023 10 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just env) <- parseInput tilesP "10/input.txt"
    let answer = enclosed env
    print answer
    -- result <- submitAnswer 2023 10 2 answer
    -- print result
    return ()
