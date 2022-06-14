module Puzzle_2020_24 where

import Automation (submitAnswer)
import Parsing
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- data types
data HexDir = E | SE | SW | W | NW | NE deriving (Eq, Ord, Show)
type HexCoord = (Int, Int, Int)

-- parsing
pathsP :: Parser [[HexDir]]
pathsP = sepBy pathP newline


pathP :: Parser [HexDir]
pathP = many directionP


directionP :: Parser HexDir
directionP = try (string "e"  >> return E ) <|>
             try (string "se" >> return SE) <|>
             try (string "sw" >> return SW) <|>
             try (string "w"  >> return W ) <|>
             try (string "nw" >> return NW) <|>
             try (string "ne" >> return NE)

-- functions
location :: [HexDir] -> HexCoord
location path = foldl move (0,0,0) path


move :: HexCoord -> HexDir -> HexCoord
move (q, r, s) dir = case dir of 
                        E  -> (q+1, r, s-1)
                        SE -> (q, r+1, s-1)
                        SW -> (q-1, r+1, s)
                        W  -> (q-1, r, s+1)
                        NW -> (q, r-1, s+1)
                        NE -> (q+1, r-1, s)


countTiles :: [[HexDir]] -> Int
countTiles paths = S.size (tileSet paths)


tileSet :: [[HexDir]] -> Set HexCoord
tileSet paths = foldl toggle S.empty paths
  where toggle :: Set HexCoord -> [HexDir] -> Set HexCoord
        toggle tiles path
            | S.member tile tiles = S.delete tile tiles
            | otherwise           = S.insert tile tiles
              where tile = location path



-- part b
exhibit :: Set HexCoord -> Int -> Set HexCoord
exhibit lobby 0 = lobby
exhibit lobby days = exhibit (update lobby) (days-1)


-- neighbours lets you just anything which is a neighbour of a black point
update :: Set HexCoord -> Set HexCoord
update lobby = M.keysSet $ M.filter id updated
  where nbhds = map builder $ S.toList lobby
        builder :: HexCoord -> Map HexCoord Int
        builder point = M.fromList $ zip (neighbours point) (repeat 1)
        counts = M.unionsWith (+) nbhds
        updated = M.mapWithKey helper counts
        helper :: HexCoord -> Int -> Bool
        helper point nbs = automataRule (S.member point lobby) nbs


automataRule :: Bool -> Int -> Bool
automataRule isBlack nbs
    | isBlack   = nbs == 1 || nbs == 2
    | otherwise = nbs == 2


neighbours :: HexCoord -> [HexCoord]
neighbours point = map (move point) [E, SE, SW, W, NW, NE]



-- mains

mainA :: IO ()
mainA = do
    (Just paths) <- parseInput pathsP "24/input.txt"
    let answer = countTiles paths
    print answer
    -- result <- submitAnswer 2020 24 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just paths) <- parseInput pathsP "24/input.txt"
    let lobby = tileSet paths
    let answer = S.size $ exhibit lobby 100
    print answer
    -- result <- submitAnswer 2020 24 2 answer
    -- print result
    return ()
