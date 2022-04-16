module Puzzle_2018_15 where

-- import Control.Lens ((&), (^.), (.~), (%~))
import Data.Sort
import Data.Map.Strict (Map)
import Data.Massiv.Array (Array, Ix2(..), Sz(..), Sz2)
import Data.Set (Set)
import qualified Data.Map.Strict as M
import qualified Data.Massiv.Array as A
import qualified Data.Set as S
import System.IO
import Automation (submitAnswer)

-- data types
type Array2 a = Array A.B A.Ix2 a
type BattleField = Array2 Tile
type BFS = Map Ix2 (Tile, Int)
data Unit = Unit {hp :: Int, damage :: Int, faction :: Faction} deriving (Eq, Show)
data Tile =  Empty | Wall | UnitTile Unit deriving (Eq)
data Faction = Elf | Goblin deriving (Eq)
data Status = Concluded | Ongoing deriving (Eq, Show)


instance Show Faction where
    show Elf = "E"
    show Goblin = "G"


instance Show Tile where
    show Empty = "."
    show Wall = "#"
    show (UnitTile u) = show $ faction u

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
    let grid = A.fromLists' A.Seq ls
    let bf = battleFieldSetup grid
    return bf 


battleFieldSetup :: Array2 Char -> BattleField
battleFieldSetup grid = A.computeAs A.B $ A.map setup grid where
    setup :: Char -> Tile
    setup c = case c of '.' -> Empty
                        '#' -> Wall
                        'E' -> UnitTile baseElf
                        'G' -> UnitTile baseGoblin



-- functions
modifyField :: BattleField -> Ix2 -> Tile -> BattleField
modifyField bf ix t = A.withMArrayST_ bf $ \mbf -> A.write_ mbf ix t


resetHealth :: BattleField -> BattleField
resetHealth bf = A.computeAs A.B $ A.map healer bf where
    healer :: Tile -> Tile
    healer (UnitTile u)     = UnitTile (Unit (baseHP) (damage u) (faction u))
    healer t                = t


turnOrder :: BattleField -> [Ix2]
turnOrder bf = A.ifoldrS pluckUnitIX [] bf where
    pluckUnitIX :: Ix2 -> Tile -> [Ix2] -> [Ix2]
    pluckUnitIX ix t acc
        | unitDetector t == True    = ix : acc
        | otherwise                 = acc


wallDetector :: Tile -> Bool
wallDetector Wall   = True
wallDetector _      = False


emptyDetector :: Tile -> Bool
emptyDetector Empty  = True
emptyDetector _      = False


unitDetector :: Tile -> Bool
unitDetector (UnitTile _) = True
unitDetector _            = False


foeDetector :: Faction -> Tile -> Bool
foeDetector fa (UnitTile u) = fa /= (faction u)
foeDetector fa _            = False


neighbours :: Ix2 -> [Ix2]
neighbours (Ix2 i j) = [i-1 :. j, i :. j-1, i :. j+1, i+1 :. j]


inBounds :: Sz2 -> Ix2 -> Bool
inBounds (Sz2 mi mj) (Ix2 i j) = (i >= 0) && (i < mi) && (j >= 0) && (j < mj)


adjacentIndices :: BattleField -> Ix2 -> [Ix2]
adjacentIndices bf ix = filter (inBounds (A.size bf)) $ neighbours ix


adjacentTiles :: BattleField -> Ix2 -> [Tile]
adjacentTiles bf ix = map ((A.!) bf) $ adjacentIndices bf ix




-- does there exist attackable neighbour?
-- does there exist any target? if so move and go back
-- 
turn :: BattleField -> Ix2 -> (BattleField, Status)
turn bf ix
    | nonUnit           = (bf, Ongoing)
    | A.any fd bf       = (attacker bf' ix', Ongoing)
    | otherwise         = (bf, Concluded)
    where
        nonUnit = not $ unitDetector (bf A.! ix)
        (UnitTile unit) = bf A.! ix
        fd = foeDetector (faction unit)
        (bf', ix') = move bf ix
        attacker :: BattleField -> Ix2 -> BattleField
        attacker b i
            | length nbFoes > 0 = attack b i (head orderedFoes)
            | otherwise         = b
            where
                nbFoes = filter (\x -> fd (b A.! x)) $ adjacentIndices b i
                orderedFoes = sortOn (\x -> (getHp $ (b A.! x), x)) nbFoes
                getHp (UnitTile u) = hp u


attack :: BattleField -> Ix2 -> Ix2 -> BattleField
attack bf attackIX defendIX
    | hp defender' > 0  = modifyField bf defendIX (UnitTile defender')
    | otherwise         = modifyField bf defendIX Empty
    where 
        (UnitTile attacker) = bf A.! attackIX
        (UnitTile defender) = bf A.! defendIX
        newHealth = (hp defender) - (damage attacker)
        defender' = Unit newHealth (damage defender) (faction defender)


-- identify closest foe through bfs
-- identify square to move towards
-- identify first step
-- take first step
move :: BattleField -> Ix2 -> (BattleField, Ix2)
move bf ix
    | length nbFoes > 0  || M.size targets == 0 = (bf, ix)
    | otherwise                                 = (moveTo bf ix ix', ix')
    where
        (UnitTile unit) = bf A.! ix
        fd = foeDetector (faction unit)
        nbFoes = filter (\x -> fd (bf A.! x)) $ adjacentIndices bf ix
        searched = bfs bf ix
        tir = targetsInRange bf (faction unit)
        targets = M.restrictKeys searched tir
        sortedTargets = sortOn (\x -> (snd . snd $ x, fst x)) $ M.toList targets
        ix' = findPath searched (fst . head $ sortedTargets)


targetsInRange :: BattleField -> Faction -> Set Ix2
targetsInRange bf f = A.ifoldrS foePlucker S.empty bf where
    foePlucker :: Ix2 -> Tile -> Set Ix2 -> Set Ix2
    foePlucker ix t acc
        | foeDetector f t   = S.union acc (S.fromList $ neighbours ix)
        | otherwise         = acc


-- move arg2 to arg3
moveTo :: BattleField -> Ix2 -> Ix2 -> BattleField
moveTo bf ix1 ix2 = A.withMArrayST_ bf $ \marr -> do
    t1 <- A.readM marr ix1
    t2 <- A.readM marr ix2
    A.write_ marr ix1 t2
    A.write_ marr ix2 t1


-- given (ix, dist)
-- for those adjacent spots which are empty and not searched, add to end of list
bfs :: BattleField -> Ix2 -> BFS 
bfs bf ix = bfsHelper (zip (neighbours ix) (repeat 1)) M.empty where
    bfsHelper :: [(Ix2, Int)] -> BFS -> BFS
    bfsHelper [] searched = searched
    bfsHelper ((ix, dist):ls) searched
        | isEmpty && isUnsearched   = bfsHelper (ls ++ nbs) (M.insert ix (bf A.! ix, dist) searched)
        | otherwise                 = bfsHelper ls searched
        where
            isEmpty = emptyDetector (bf A.! ix)
            isUnsearched = not (M.member ix searched)
            nbs = zip (adjacentIndices bf ix) (repeat (dist+1))



-- if distance one that's the square you want
-- else find the minimum ix among those which are adjacent to ix and have dist = one less
findPath :: BFS -> Ix2 -> Ix2
findPath searched ix
    | dist == 1 = ix
    | otherwise = findPath searched (fst . M.findMin $ options)
    where
        (tile, dist) = searched M.! ix
        options = M.filterWithKey pathFinder searched
        pathFinder = \k a -> k `elem` (neighbours ix) && (snd a) == (dist - 1)





battleRound :: BattleField -> (BattleField, Status)
battleRound bf = battleHelper bf turnQueue where
    turnQueue = turnOrder bf
    battleHelper :: BattleField -> [Ix2] -> (BattleField, Status)
    battleHelper bf [] = (bf, Ongoing)
    battleHelper bf (ix:ixs)
        | status == Concluded   = (bf', Concluded)
        | otherwise             = battleHelper bf' ixs
        where 
            (bf', status) = turn bf ix


simulateBattle :: BattleField -> Int -> (BattleField, Int)
simulateBattle bf time
    | status == Concluded   = (bf', time)
    | otherwise             = simulateBattle bf' (time+1)
    where (bf', status) = battleRound bf


nRounds :: BattleField -> Int -> BattleField
nRounds bf 0 = bf
nRounds bf n = nRounds (fst $ battleRound bf) (n-1)


outcome :: BattleField -> Int
outcome bf = time * (totalHP bf') where
    (bf', time) = simulateBattle bf 0


totalHP :: BattleField -> Int
totalHP bf = A.foldlS hpSum 0 bf where
    hpSum :: Int -> Tile -> Int
    hpSum acc (UnitTile u)  = acc + (hp u)
    hpSum acc _             = acc


hpExtractor :: BattleField -> [Int]
hpExtractor bf = A.foldrS hpPlucker [] bf where
    hpPlucker :: Tile -> [Int] -> [Int]
    hpPlucker (UnitTile u) acc = (hp u) : acc
    hpPlucker _ acc = acc


extractElves :: BattleField -> [Unit]
extractElves bf = A.foldrS elfPlucker [] bf where
    elfPlucker :: Tile -> [Unit] -> [Unit]
    elfPlucker (UnitTile u) acc
        | faction u == Elf  = u : acc
        | otherwise         = acc
    elfPlucker t acc = acc

increaseDamage :: BattleField -> Faction -> BattleField
increaseDamage bf f = A.computeAs A.B $ A.map (weaponize f) bf where
    weaponize :: Faction -> Tile -> Tile
    weaponize f (UnitTile u)
        | faction u == f    = UnitTile (Unit (hp u) (damage u + 1) f)
        | otherwise         = UnitTile u
    weaponize f t = t


noDeathsOutcome :: BattleField -> Int
noDeathsOutcome bf
    | startingElves == endingElves  = outcome bf
    | otherwise                     = noDeathsOutcome $ increaseDamage bf Elf
    where
        (bf', _) = simulateBattle bf 0
        startingElves = length $ extractElves bf
        endingElves = length $ extractElves bf'

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
    bf <- parseInput "input.txt"
    let answer = noDeathsOutcome bf
    print answer
    result <- submitAnswer 2018 15 2 answer
    print result
    return ()
