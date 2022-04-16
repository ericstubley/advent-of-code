module Puzzle_2018_15 where

import Control.Lens
import Control.Monad
import Control.Monad.ST
import Data.Map.Strict (Map)
import Data.Massiv.Array (Array, Ix2(..))
import qualified Data.Map.Strict as M
import qualified Data.Massiv.Array as A
import System.IO
import Automation (submitAnswer)

-- data types
type Array2 a = Array A.B A.Ix2 a
type MArray2 s a = MArray s A.B A.Ix2 a
type BattleField = Array2 Tile
type MBattledField s  = MArray2 s Tile
type BFS = Map Ix2 (Tile, Int)
data Tile = Empty | Wall | Unit deriving (Eq, Show)
data Unit = Unit {hp :: Int, damage :: Int, faction :: Faction} deriving (Eq, Show)
data Faction = Elf | Goblin deriving (Eq, Show)
data Status = Concluded | Ongoing deriving (Eq, Show)

baseHP :: Int
baseHP = 200

baseDamage :: Int
baseDamage = 3

baseElf :: Unit
baseElf = Unit baseHP baseDamage Elf

baseGoblin :: Unit
baseGoblin = Unit baseHP baseDamage Goblin


-- parsing

parseInput :: String -> IO BattleField
parseInput filename = do
    raw <- readFile filename
    let ls = lines raw
    let grid = A.fromLists' A.seq ls
    let bf = battleFieldSetup grid
    return bf 


battleFieldSetup :: Array2 Char -> BattleField
battleFieldSetup grid = A.map setup grid where
    setup :: Char -> Tile
    setup c = case c of '.' -> Empty
                        '#' -> Wall
                        'E' -> baseElf
                        'G' -> baseGoblin



-- functions

resetHealth :: BattleField -> BattleField
resetHealth bf = A.map healer bf where
    healer :: Tile -> Tile
    healer u@(Unit _ _ _)   = u & hp .~ baseHP
    healer t                = t


turnOrder :: BattleField -> [Ix2]
turnOrder bf = A.ifoldlS pluckUnitIX [] bf where
    pluckUnitIX :: [Ix2] -> Ix2 -> Tile -> [Ix2]
    pluckUnitIX acc ix t
        | unitDetector t == True    = ix : acc
        | otherwise                 = acc


wallDetector :: Tile -> Bool
wallDetector Wall   = True
wallDetector _      = False


emptyDetector :: Tile -> Bool
emptyDetector Empty  = True
emptyDetector _      = False


unitDetector :: Tile -> Bool
unitDetector (Unit _ _ _)   = True
unitDetector _              = False


foeDetector :: Faction -> Tile -> Bool
foeDetector fa (Unit _ _ f) = (fa == f)
foeDetector fa _            = False


neighbours :: Ix2 -> [Ix2]
neighbours (Ix2 i j) = [i-1 :. j, i :. j-1, i :. j+1, i+1 :. j]


inBounds :: Sz2 -> Ix2
inBounds (Sz2 mi mj) (Ix2 i j) = (i >= 0) && (i < mi) && (j >= 0) && (j < mj)


adjacentIndices :: MBattledField s -> Ix2 -> [Ix2]
adjacentIndices bf ix = filter (inBounds (sizeOfMArray bf)) $ neighbours ix


adjacentTiles :: MBattledField s -> Ix2 -> [Tile]
adjacentTiles bf ix = map (A.readM bf) $ adjacentIndices bf ix




-- does there exist any targets
-- does there exist attackable neighbours
-- move to closest neighbour
turn :: MBattledField s -> Ix2 -> ST s (MBattledField s, Status)
turn bf ix = do
    unit <- A.readM bf ix
    let fd = foeDetector (faction unit))
    if A.any fd bf
        then 
            let af = filter fd $ adjacentTiles
            if length af > 0
                then (attack bf ix (head af), Ongoing)
                else (move bf ix, Ongoing)
        else (mbf, Concluded)


attack :: MBattledField s -> Ix2 -> Ix2 -> ST s (MBattledField s)
attack bf attackerIX defenderIX = do
    attacker <- A.readM bf attackerIX
    defender <- A.readM bf defenderIX
    let defender' = defender & hp %~ (\x -> x - (attacker ^. damage))
    let remainder = defender' ^. hp
    if remainder > 0
        then A.write_ bf defenderIX defender'
        else A.write_ bf defenderIX Empty


-- identify closest foe through bfs
-- identify square to move towards
-- identify first step
-- take first step
move :: MBattledField s -> Ix2 -> ST s (MBattledField s)
move bf ix = do
    unit <- A.readM bf ix
    let fd = foeDetector (faction unit))
    let searched = bfs bf ix
    let targets = M.filter (\x -> fd $ fst x) searched
    if length targets > 0
        then 
            let dest = head $ findPath searched (M.findMin targets)
        else 




-- given (ix, dist)
-- for those adjacent spots which are empty and not searched, add to end of list
bfs :: MBattledField s -> Ix2 -> BFS 
bfs bf ix = bfsHelper [(ix, 0)] M.empty where
    bfsHelper :: [Ix2] -> BFS -> BFS
    bfsHelper [] searched               = searched
    bfsHelper ((ix, dist):ls) searched
        | emptyDetector (A.readM bf)
        |
        |


findPath :: BFS -> Ix2 -> Ix2
findPath searched ix
    | 
    |
    where
        (tile, dist)


battleRound :: BattleField -> (BattleField, Status)
battleRound bf = runST $ do battleHelper mbf to where
    mbf = A.thaw bf
    to = turnOrder bf
    battleHelper :: MBattledField s -> [Ix2] -> ST s (BattledField, Status)
    battleHelper mbf [] = mbf
    battleHelper mbf ix:ixs = do
        (mbf', status) <- turn mbf ix
        if status == Concluded
            then (A.freezeS mbf', status)
            else battleHelper mbf' ixs


simulateBattle :: BattleField -> Int -> BattleField
simulateBattle bf time
    | status == Concluded   = (bf', time)
    | otherwise             = simulateBattle bf' (time+1)
    where (bf', status) = battleRound bf


outcome :: BattleField -> Int
outcome bf = time * (totalHP bf') where
    (time, bf') = simulateBattle bf 0


totalHP :: BattleField -> Int
totalHP bf = A.foldlS hpSum 0 bf where
    hpSum :: Int -> Tile -> Int
    hpSum acc u@(Unit _ _ _)    = acc + (u ^. hp)
    hpSum acc _                 = acc


-- main functions


mainA :: IO ()
mainA = do
    bf <- parseInput "input.txt"
    let answer = outcome bf
    print answer
    -- result <- submitAnswer 2018 15 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    let answer = 0
    print answer
    -- result <- submitAnswer 2018 15 2 answer
    -- print result
    return ()
