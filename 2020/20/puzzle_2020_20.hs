module Puzzle_2020_20 where

import Automation (submitAnswer)
import Parsing
import Data.Massiv.Array (Array, Ix2(..), Ix1(..))
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Massiv.Array as A
import qualified Data.Map.Strict as M
import qualified Data.Set as S


-- data types
type Array2 a = A.Array A.U A.Ix2 a
type Row a = A.Array A.U A.Ix1 a

data Tile = Tile {ident :: Int, tile :: Array2 Char} deriving (Eq, Ord, Show)

-- parsing
tilesP :: Parser (Map Int Tile)
tilesP = constructor <$> sepBy tileP newline
  where constructor :: [Tile] -> Map Int Tile
        constructor ts = M.fromList $ map (\x -> (ident x, x)) ts


tileP :: Parser Tile
tileP = do
    string "Tile "
    i <- integer
    char ':'
    newline
    t <- gridP
    return $ Tile i t


gridP :: Parser (Array2 Char)
gridP = A.fromLists' A.Seq <$> count 10 rowP


rowP :: Parser [Char]
rowP = count 10 cellP <* newline


cellP :: Parser Char
cellP = char '#' <|> char '.'


-- part a functions
corners :: Map Int Tile -> [Int]
corners m = M.keys $ M.filter (==2) (matchCounts m)


matchCounts :: Map Int Tile -> Map Int Int
matchCounts m = M.map (\t -> M.size (matches m t)) m


matches :: Map Int Tile -> Tile -> Map Int Tile
matches m t = M.filter (align t) m


align :: Tile -> Tile -> Bool
align (Tile i1 t1) (Tile i2 t2)
    | i1 == i2  = False
    | otherwise = not $ S.null (S.intersection e1 e2)
      where e1 = edges (Tile i1 t1)
            e2 = edges (Tile i2 t2)


edges :: Tile -> Set (Row Char)
edges (Tile _ t) = result
  where sliceIxs = [(1, 0), (1, 9), (2, 0), (2, 9)]
        slices = map (\s -> t A.<!> s) sliceIxs
        edgeLs = slices ++ (map (A.reverse A.Dim1) slices)
        result = S.fromList $ map A.compute edgeLs


-- part b functions
d8 :: [(Array2 Char -> Array2 Char)]
d8 = [id, rot, rot2, rot3, over, over . rot, over . rot2, over . rot3]
  where rot = A.compute . (A.reverse A.Dim1) . A.transpose 
        rot2 = rot.rot
        rot3 = rot.rot.rot
        over = A.compute . A.transpose


transforms :: Array2 Char -> [Array2 Char]
transforms grid = map (\t -> t grid) d8


seaMonster :: Array2 Char
seaMonster = A.fromLists' A.Seq m
  where m = [ "                  # "
            , "#    ##    ##    ###"
            , " #  #  #  #  #  #   "]


monsterIxs :: [Ix2]
monsterIxs = [(i:.j) | i <- [0,1,2], j <- [0..19]]


monsterStencil :: A.Stencil A.Ix2 Char Bool
monsterStencil = A.makeStencil (A.Sz (3:.20)) (0:.0) go
  where go :: (A.Ix2 -> Char) -> Bool
        go get = all comp monsterIxs
          where comp ix
                    | mc == ' '              = True
                    | mc == '#' && gc == '#' = True
                    | otherwise              = False
                      where mc = seaMonster A.! ix
                            gc = get ix


monsterCount :: Array2 Char -> Int
monsterCount grid = gridCount marked True
  where marked = A.compute $ A.applyStencil A.noPadding monsterStencil grid


-- 15 # in a monster
-- so if they don't overlap, then roughness = number of # - 15 x number of monsters
roughness :: Map Int Tile -> Int
roughness m = (gridCount grid '#') - 15*totalMonsters
  where grid = layout m
        totalMonsters = sum $ map monsterCount (transforms grid)


-- count the number of elements in the grid which are equal to a
gridCount :: (A.Unbox a, Eq a) => Array2 a -> a -> Int
gridCount grid a = A.foldlS (\acc e -> if a == e then (acc + 1) else acc) 0 grid

layout :: Map Int Tile -> Array2 Char
layout m = squash . (M.map trim) . orient . assign $ m


interleave :: [a] -> [a] -> [a]
interleave [] bs = bs
interleave as [] = as
interleave (a:as) (b:bs) = a : b : interleave as bs


assign :: Map Int Tile -> Map Ix2 Tile
assign m = foldl helper m' ixs
  where d = floor . sqrt . fromIntegral $ M.size m
        row0 = [0:.j | j <- [2..(d-1)]]
        row1 = [1:.j | j <- [1..(d-1)]]
        rest = [i:.j | i <- [2..(d-1)], j <- [0..(d-1)]]
        ixs = (interleave row1 row0) ++ rest
        info = M.map (matches m) m
        ident00 = head $ corners m
        ident01 = head $ M.keys $ info M.! (ident tile00)
        ident10 = last $ M.keys $ info M.! (ident tile00)
        tile00 = m M.! ident00
        tile01 = m M.! ident01
        tile10 = m M.! ident10
        m' = M.fromList [(0:.0, tile00), (0:.1, tile01), (1:.0, tile10)]
        -- given an index, find the (hopefully unique) thing to put there
        -- assuming everything above and to the left is done, the thing we're
        --      looking for is the unique ident which
        --      a) is not already in acc
        --      b) is in the matches for (i-1):.j
        --      c) is in the matches for i:.(j-1)
        helper :: Map Ix2 Tile -> Ix2 -> Map Ix2 Tile
        helper acc (i:.j) = M.insert (i:.j) matchingTile acc
          where matchingTile = head $ filter (\x -> not $ elem x (M.elems acc)) found
                found
                    | i == 0    = leftMatches
                    | j == 0    = aboveMatches
                    | otherwise = [x | x <- leftMatches, y <- aboveMatches, x == y]
                      where left = acc M.! (i:.(j-1))
                            leftMatches = M.elems $ info M.! (ident left)
                            above = acc M.! ((i-1):.j)
                            aboveMatches = M.elems $ info M.! (ident above)



orient :: Map Ix2 Tile -> Map Ix2 Tile
orient m = foldl helper m' ixs
  where d = floor . sqrt . fromIntegral $ M.size m
        ixs = [i:.j | i <- [0..(d-1)], j <- [0..(d-1)]
                    , (i, j) /= (0, 0)]
        grid00 = m M.! (0:.0)
        grid01 = m M.! (0:.1)
        grid10 = m M.! (1:.0)
        t00 = tile grid00
        t01 = tile grid01
        t10 = tile grid10
        initial = [f t00 | f <- d8, g <- d8, h <- d8
                         , checkOrientation (f t00) (1:.0) (g t10)
                         , checkOrientation (f t00) (0:.1) (h t01)]
        m' = M.singleton (0:.0) (Tile (ident grid00) (head initial))
        helper :: Map Ix2 Tile -> Ix2 -> Map Ix2 Tile
        helper acc (i:.j) = M.insert (i:.j) t' acc
          where dir = if j == 0 then (1:.0) else (0:.1)
                prev = if j == 0 then (acc M.! ((i-1):.0)) else (acc M.! (i:.(j-1)))
                t = m M.! (i:.j)
                t' = Tile (ident t) (orienter (tile prev) dir (tile t))




-- ix is the direction t1 -> t2
-- should only ever be down (1:.0) or right (0:.1)
-- find the orientation of t2 that matches the current orientation of t1
orienter :: Array2 Char -> Ix2 -> Array2 Char -> Array2 Char
orienter t1 ix t2 = t2'
  where t2' = head $ filter (checkOrientation t1 ix) (transforms t2)

-- ix is the direction t1 -> t2
-- should only even be down (1:.0) or right (0:.1)
-- find the orientation of t2 that matches the current orientation of t1
checkOrientation :: Array2 Char -> Ix2 -> Array2 Char -> Bool
-- bottom row of t1 == top row of t2
checkOrientation t1 (1:.0) t2 = (t1 A.<!> (2, 9)) == (t2 A.<!> (2, 0))
-- right row of t1 == left row of t2
checkOrientation t1 (0:.1) t2 = (t1 A.<!> (1, 9)) == (t2 A.<!> (1, 0))
checkOrientation _ _ _ = error "unexpected case"



trim :: Tile -> Tile
trim (Tile i t) = Tile i t'
  where (A.Sz (d:.e)) = A.size t
        t' = A.compute $ A.extract' (1:.1) (A.Sz ((d-2):.(e-2))) t


squash :: Map Ix2 Tile -> Array2 Char
squash m = grid
  where d = floor . sqrt . fromIntegral $ M.size m
        row :: Int -> Array2 Char
        row i = A.compute $ A.concat' (A.Dim 1) [tile (m M.! (i:.j)) | j <- [0..(d-1)]]
        grid = A.compute $ A.concat' (A.Dim 2) [row i | i <- [0..(d-1)]]


-- mains

mainA :: IO ()
mainA = do
    (Just tiles) <- parseInput tilesP "20/input.txt"
    let answer = product (corners tiles)
    print answer
    -- result <- submitAnswer 2020 20 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just tiles) <- parseInput tilesP "20/input.txt"
    let answer = roughness tiles
    print answer
    -- result <- submitAnswer 2020 20 2 answer
    -- print result
    return ()
