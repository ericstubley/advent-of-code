module Puzzle_2022_14 where

import Automation (submitAnswer)
import Parsing
import Data.Map (Map)
import qualified Data.Map as M
import Data.Massiv.Array (Ix2(..))

-- data types
data Tile = Air | Sand | Rock deriving (Eq, Ord, Show)

data FillMode = Abyss | Floor deriving (Eq, Ord, Show)

type Cavern = Map Ix2 Tile

-- parsing
cavernP :: Parser Cavern
cavernP = M.unions <$> (sepBy lineP newline)


lineP :: Parser Cavern
lineP = (M.fromList . (map (\c -> (c, Rock))) . makePath) 
        <$> (sepBy coordP (string " -> "))


coordP :: Parser Ix2
coordP = do
    j <- natural
    char ','
    i <- natural
    return $ j :. i


makePath :: [Ix2] -> [Ix2]
makePath [] = []
makePath (x:[]) = []
makePath (x:y:xs) = (path x y) ++ (makePath (y:xs))


path :: Ix2 -> Ix2 -> [Ix2]
path (j1:.i1) (j2:.i2) = [(j:.i) | j <- [minJ..maxJ], i <- [minI..maxI]]
  where minJ = min j1 j2
        maxJ = max j1 j2
        minI = min i1 i2
        maxI = max i1 i2

-- functions
source :: Ix2
source = 500:.0


abyss :: Cavern -> Int
abyss cavern = maximum . map depth . M.keys . M.filter (==Rock) $ cavern
  where depth (j:.i) = i


fill :: FillMode -> Cavern -> Cavern
fill mode cavern = fill' cavern
  where level = abyss cavern
        fill' :: Cavern -> Cavern
        fill' cavern
            | mode == Floor && M.member source cavern = cavern
            | mode == Abyss && cavern' == cavern      = cavern
            | otherwise                               = fill' cavern'
              where cavern' = grain mode level cavern


grain :: FillMode -> Int -> Cavern -> Cavern
grain mode level cavern = go source
  where go :: Ix2 -> Cavern
        go (j:.i)
            | mode == Floor && i > level          = M.insert (j:.i) Sand cavern
            | mode == Abyss && i > level          = cavern
            | M.notMember (j     :. (i+1)) cavern = go (j:.(i+1))
            | M.notMember ((j-1) :. (i+1)) cavern = go ((j-1):.(i+1))
            | M.notMember ((j+1) :. (i+1)) cavern = go ((j+1):.(i+1))
            | otherwise                           = M.insert (j:.i) Sand cavern


abyssVolume :: Cavern -> Int
abyssVolume = M.size . M.filter (== Sand) . fill Abyss


floorVolume :: Cavern -> Int
floorVolume = M.size . M.filter (== Sand) . fill Floor

-- mains

mainA :: IO ()
mainA = do
    (Just cavern) <- parseInput cavernP "14/input.txt"
    let answer = abyssVolume cavern
    print answer
    -- result <- submitAnswer 2022 14 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just cavern) <- parseInput cavernP "14/input.txt"
    let answer = floorVolume cavern
    print answer
    -- result <- submitAnswer 2022 14 2 answer
    -- print result
    return ()
