module Puzzle_2018_22 where

import Data.Heap (Heap, Entry(..))
import Data.Map.Strict (Map)
import Data.Massiv.Array (Array, Ix2(..))
import Data.Vector (Vector)
import qualified Data.Heap as H
import qualified Data.Map.Strict as M
import qualified Data.Massiv.Array as A
import qualified Data.Vector as V
import System.IO
import Automation (submitAnswer)


-- data types
type Array2 a = A.Array A.U A.Ix2 a
data Gear = Neither | Torch | Climbing deriving (Eq, Ord, Enum, Show)
type CaveState = (Ix2, Gear)
type HeapEntry = Entry Int CaveState
type ScoreMap = Map CaveState Int

-- part a functions

erosionUnfold :: Int -> Ix2 -> Vector Int -> Ix2 -> (Int, Vector Int)
erosionUnfold depth target prevs (y:.x) = (erosion, prevs')
  where
    erosion = mod (geologicIndex (y:.x) + depth) 20183
    prevs' = prevs V.// [(x, erosion)]
    geologicIndex :: Ix2 -> Int
    geologicIndex (y:.x)
        | (y:.x) == (0:.0)  = 0
        | (y:.x) == target  = 0
        | y == 0            = x * 16807
        | x == 0            = y * 48271
        | otherwise         = (prevs V.! (x-1)) * (prevs V.! x)


erosionGrid :: Int -> Ix2 -> Ix2 -> Array2 Int
erosionGrid depth (ty:.tx) (sy:.sx) = A.computeAs A.U $ unfolded where
    unfolded = A.iunfoldrS_ size unfolder prevs
    size = A.Sz ((sy+1) :. (sx+1))
    unfolder = erosionUnfold depth (ty:.tx)
    prevs = V.replicate (sx+1) 0


risk :: Array2 Int -> Int
risk cave = A.sum $ A.map (\x -> x `mod` 3) cave


caveRepr :: Int -> String
caveRepr e = case (e `mod` 3) of
    0 -> "."
    1 -> "="
    2 -> "|"


-- part b functions; A*
manhattan :: Ix2 -> Ix2 -> Int
manhattan (ai:.aj) (bi:.bj) = abs (ai-bi) + abs (aj - bj)


heuristic :: CaveState -> CaveState -> Int
heuristic (pos1, gear1) (pos2, gear2) = manhattan pos1 pos2 + gearCheck
    where gearCheck = if gear1 == gear2 then 0 else 7


neighbours :: Array2 Int -> CaveState -> [CaveState]
neighbours cave ((y:.x), gear) = filter (isValid cave) options
  where
    options = zip [(y-1):.x, (y+1):.x, y:.(x-1), y:.(x+1)] (repeat gear)
    isValid :: Array2 Int -> CaveState -> Bool
    isValid cave ((y:.x), _)
        | y < 0   || x < 0          = False
        | y >= my || x >= mx        = False
        | terrain == fromEnum gear  = False
        | otherwise                 = True
          where 
            (A.Sz (my:.mx)) = A.size cave
            terrain = (cave A.! (y:.x)) `mod` 3


gearChange :: Array2 Int -> CaveState -> CaveState
gearChange cave ((y:.x), gear) = case (terrain, gear) of
    (0, Torch)      -> ((y:.x), Climbing)
    (0, Climbing)   -> ((y:.x), Torch)
    (1, Neither)    -> ((y:.x), Climbing)
    (1, Climbing)   -> ((y:.x), Neither)
    (2, Neither)    -> ((y:.x), Torch)
    (2, Torch)      -> ((y:.x), Neither)
    where terrain = (cave A.! (y:.x)) `mod` 3


edges :: Array2 Int -> CaveState -> [(CaveState, Int)]
edges cave state = (gearChange cave state, 7) : (zip (neighbours cave state) (repeat 1))


-- from wikipedia here's some notes on A*

-- helper is going to need:
--  a min heap of CaveState ordered by current known distance
--      we'll pull off the current node as the min of this heap
--  a map from CaveState -> CaveState of preceeding nodes on best path
--      only need this is we want to return the path, which we don't
--  gScore = current best known dist from start to cs
--  fScore = gScore + h


-- if the heap is empty, exit
-- else find heap element with minimal fScore (does the heap sort on fScore?)
-- if that's the target, return it's gScore
-- otherwise for each neighbour (and the gearChange)
--      is that distance better than the currently known one?
--      if yes then update all the things

aStar :: Array2 Int -> Ix2 -> Int
aStar cave target = helper openPathsInit gScoreInit fScoreInit
  where
    openPathsInit = H.singleton (Entry (manhattan target (0:.0)) (0:.0, Torch))
    gScoreInit = M.singleton (0:.0, Torch) 0
    fScoreInit = M.singleton (0:.0, Torch) (manhattan target (0:.0))
    helper :: Heap HeapEntry -> ScoreMap -> ScoreMap -> Int
    helper openPaths gScore fScore
        | curr == (target, Torch)   = f
        | otherwise                 = helper openPaths' gScore' fScore'
          where
            (Entry f curr) = H.minimum openPaths
            g = scoreFind gScore curr
            h :: CaveState -> Int
            h = heuristic (target, Torch)
            viableEdges = filter isViable (edges cave curr) where
                isViable :: (CaveState, Int) -> Bool
                isViable (cs, dist) = (g + dist) < (scoreFind gScore cs)
            ns = map fst viableEdges
            ds = map snd viableEdges
            gs = map (+g) ds
            fs = zipWith (\n g -> h n + g) ns gs
            gScore' = M.union (M.fromList $ zip ns gs) gScore
            fScore' = M.union (M.fromList $ zip ns fs) fScore
            filteredPaths = H.filter (\he -> not $ (payload he) `elem` ns) $ H.deleteMin openPaths
            newPaths = H.fromList $ zipWith (Entry) fs ns
            openPaths' = H.union newPaths filteredPaths

-- filter the edges to viable edges by tentative gScore < found gScore
-- for each leftover: 
--      update each scoremap (maybe adding in)
--      make a new set of paths, without curr and with each of the neighbours
-- viableEdges = filter by tentative 

scoreFind :: ScoreMap -> CaveState -> Int
scoreFind scores state = M.findWithDefault big state scores

big :: Int
big = maxBound :: Int


fastestRoute :: Int -> Ix2 -> Ix2 -> Int
fastestRoute depth target scaler = aStar bigCave target where
    bigCave = erosionGrid depth target scaler

-- printing

printableGrid :: (A.Unbox a) => (a -> String) -> Array2 a -> [String]
printableGrid repr grid = map mapRow [0..(mi-1)] where
    (A.Sz (mi:.mj)) = A.size grid
    mapRow :: Int -> String
    mapRow i = concat $ map (\j -> repr (grid A.! (i:.j))) [0..(mj-1)]


printGrid :: (A.Unbox a) => (a -> String) -> Array2 a -> IO ()
printGrid repr grid = putStrLn "" >> mapM_ putStrLn (printableGrid repr grid)


-- mains

mainA :: IO ()
mainA = do
    let answer = risk $ erosionGrid 10647 (770:.7) (770:.7)
    print answer
    -- result <- submitAnswer 2018 22 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    let answer = fastestRoute 10647 (770:.7) (800:.35)
    print answer
    -- result <- submitAnswer 2018 22 2 answer
    -- print result
    return ()
