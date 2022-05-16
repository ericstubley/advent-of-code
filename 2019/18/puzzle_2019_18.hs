module Puzzle_2019_18 where

import Automation (submitAnswer)
import Grid
import Parsing
import Control.Monad (sequence)
import Control.Monad.State (get, put, evalState)
import Data.Char (isLower, isUpper, toLower, toUpper, isNumber)
import Data.Heap (Heap, Entry(..))
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Sequence (Seq(..), (><))
import qualified Control.Monad.State as St
import qualified Data.Massiv.Array as A
import qualified Data.Heap as H
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Sequence as Sq


-- data types
data Env = Env
    { maze :: Array2 Char
    , keyMap :: Map Char Ix2
    , doorMap :: Map Char Ix2
    , start :: Ix2 } 
    deriving (Eq, Ord, Show)

data EnvB = EnvB
    { mazeB :: Array2 Char
    , keyMapB :: Map Char Ix2
    , doorMapB :: Map Char Ix2
    , starts :: [Ix2] } 
    deriving (Eq, Ord, Show)

-- type HeapEntry = Entry Int (Set Char, Ix2)
type HeapEntry = Entry Int (Set Char, Char)

type StateB = (Set Char, Char, Char, Char, Char)
type HeapEntryB = Entry Int StateB

type Graph = Map (Char, Char) (Set Char, Int)


-- parsing
mazeP :: Parser (Array2 Char)
mazeP = do
    cs <- many asciiChar
    return $ A.fromLists' A.Seq (lines cs)


-- functions

buildEnvironment :: Array2 Char -> Env
buildEnvironment grid = Env grid ks ds s where
    (ks, ds, s) = A.ifoldlS extractor (M.empty, M.empty, (0:.0)) grid
    extractor (k, d, p) (i:.j) c
        | isLower c = (M.insert c (i:.j) k, d, p)
        | isUpper c = (k, M.insert c (i:.j) d, p)
        | c == '@'  = (k, d, (i:.j))
        | otherwise = (k, d, p)


buildEnvironmentB :: Array2 Char -> EnvB
buildEnvironmentB grid = EnvB grid ks ds ss where
    (ks, ds, ss) = A.ifoldlS extractor (M.empty, M.empty, []) grid
    extractor (k, d, ps) (i:.j) c
        | isLower c = (M.insert c (i:.j) k, d, ps)
        | isUpper c = (k, M.insert c (i:.j) d, ps)
        | isNumber c  = (k, d, (i:.j) : ps)
        | otherwise = (k, d, ps)


-- build a graph which sends a pair of keys to the shortest distance between
-- them and the list of doors blocking that path
-- also put @ in the graph
buildGraph :: Env -> Graph
buildGraph env = M.unions $ map (distsFromVertex (maze env)) ((start env) : M.elems (keyMap env))


buildGraphB :: EnvB -> Graph
buildGraphB env = M.unions $ map (distsFromVertex (mazeB env)) (starts env ++ M.elems (keyMapB env))



distsFromVertex :: Array2 Char -> Ix2 -> Graph
distsFromVertex grid (i:.j) = go initQueue S.empty M.empty where
    initQueue = Sq.singleton (i:.j, (S.empty, 0))
    key = grid A.! (i:.j)
    -- takes a queue, seen things, graph -> graph
    -- cases: queue is empty        -> return
    --      head of queue seen      -> continue
    --      is new key              -> add the graph, continue
    --      is door                 -> add to doors for this path, continue
    --      is empty or start       -> 
    go :: Seq (Ix2, (Set Char, Int)) -> Set Ix2 -> Graph -> Graph
    go Empty _ graph = graph
    go ((p, (doors, dist)):<|q) seen graph
        | S.member p seen = go q seen graph
        | isLower c && (not $ M.member (key, c) graph) = go q' seen' graph'
        | isUpper c = go q'' seen' graph
        | c == '.' = go q' seen' graph
        | c == '@' = go q' seen' graph
        | isNumber c = go q' seen' graph
        | otherwise = go q seen' graph
        where
            c = grid A.! p
            seen' = S.insert p seen
            graph' = M.insert (key, c) (doors, dist) graph
            q' = q >< (Sq.fromList $ map (\x -> (x, (doors, dist+1))) (neighbours p))
            q'' = q >< (Sq.fromList $ map (\x -> (x, (S.insert (toLower c) doors, dist+1))) (neighbours p))


findKeys :: Array2 Char -> Int
findKeys grid = go initHeap S.empty where
    graph = buildGraph $ buildEnvironment grid
    allKeys = S.fromList $ map fst (M.keys graph)
    initHeap = H.singleton (Entry 0 (S.singleton '@', '@'))
    -- go takes in a queue (in heap form) of explorations to process
    -- a set of states explored from so far to avoid duplication
    -- 
    go :: Heap HeapEntry -> Set (Set Char, Char) -> Int
    go heap seen
        | S.member (keys, pos) seen = go (H.deleteMin heap) seen
        | keys == allKeys           = d
        | otherwise                 = go heap' seen'
        where
            (Entry d (keys, pos)) = H.minimum heap
            opts = pathways graph keys pos
            payloads = map (\x -> (S.insert x keys, x)) opts
            payloads' = filter (\x -> not $ S.member x seen) payloads
            dists = map (\x -> snd (graph M.! (pos, snd x))) payloads'
            entries = zipWith (\x y -> Entry (d + x) y) dists payloads'
            heap' = H.union (H.deleteMin heap) (H.fromList entries)
            seen' = S.insert (keys, pos) seen



findKeysB :: Array2 Char -> Int
findKeysB grid = go initHeap S.empty where
    graph = buildGraphB $ buildEnvironmentB grid
    allKeys = S.fromList $ map fst (M.keys graph)
    initState = (S.fromList "1234", '1', '2', '3', '4')
    initHeap = H.singleton (Entry 0 initState)
    -- go takes in a queue (in heap form) of explorations to process
    -- a set of states explored from so far to avoid duplication
    go :: Heap HeapEntryB -> Set StateB -> Int
    go heap seen
        | S.member (keys,x,y,z,w) seen = go (H.deleteMin heap) seen
        | keys == allKeys = d
        | otherwise       = go heap' seen'
        where
            (Entry d (keys, x, y, z, w)) = H.minimum heap
            seen' = S.insert (keys, x, y, z, w) seen
            entriesX = map (\a -> Entry (d + snd a) (S.insert (fst a) keys, fst a, y, z, w)) (movesFrom graph keys x)
            entriesY = map (\a -> Entry (d + snd a) (S.insert (fst a) keys, x, fst a, z, w)) (movesFrom graph keys y)
            entriesZ = map (\a -> Entry (d + snd a) (S.insert (fst a) keys, x, y, fst a, w)) (movesFrom graph keys z)
            entriesW = map (\a -> Entry (d + snd a) (S.insert (fst a) keys, x, y, z, fst a)) (movesFrom graph keys w)
            entries = entriesX ++ entriesY ++ entriesZ ++ entriesW
            entries' = filter (\a -> not $ S.member (H.payload a) seen) entries
            heap' = H.union (H.deleteMin heap) (H.fromList entries')


movesFrom :: Graph -> Set Char -> Char -> [(Char, Int)]
movesFrom graph keys key = final where
    opts = pathwaysB graph keys key
    dists = map (\x -> snd $ graph M.! (key, x)) opts
    final = zip opts dists


-- findKeys :: Array2 Char -> Int
-- findKeys grid = evalState (minDistance graph S.empty '@') initialCache where
--     env = buildEnvironment grid
--     graph = buildGraph env
--     allKeys = M.keys (keyMap env)
--     initialCache = M.fromList $ zip (map (\x -> (S.fromList allKeys, x)) allKeys) (repeat 0)


-- minDistance :: Graph -> Set Char -> Char -> Cache Int
-- minDistance graph keys key = do
--     cache <- get
--     if M.member (keys, key) cache
--         then return (cache M.! (keys, key))
--         -- otherwise, you need to
--         --      get the list of all keys you could go to
--         --      call minDistance for each
--         --      take the minimum
--         else do
--             let paths = pathways graph keys key
--             ds <- sequence (map (\x -> minDistance graph (S.insert x keys) x) paths)
--             let ds' = zipWith (+) ds (map (\x -> snd (graph M.! (key, x))) paths)
--             let d = if null ds' then maxBound else minimum ds'
--             put $ M.insert (keys, key) d cache
--             return d


pathways :: Graph -> Set Char -> Char -> [Char]
pathways graph keys key = openKeys where
    allKeys = S.fromList (map snd $ filter (\x -> fst x == '@' && snd x /= '@') (M.keys graph))
    missingKeys = S.difference allKeys keys
    isOpen :: Char -> Bool
    isOpen k = S.isSubsetOf (fst (graph M.! (key, k))) keys
    openKeys = S.toList (S.filter isOpen missingKeys)


pathwaysB :: Graph -> Set Char -> Char -> [Char]
pathwaysB graph keys key = openKeys where
    componentKeys = S.fromList (map snd $ filter (\x -> fst x == key && snd x /= key) (M.keys graph))
    missingKeys = S.difference componentKeys keys
    isOpen :: Char -> Bool
    isOpen k = S.isSubsetOf (fst (graph M.! (key, k))) keys
    openKeys = S.toList (S.filter isOpen missingKeys)


neighbours :: Ix2 -> [Ix2]
neighbours (i:.j) = [(i+1):.j, (i-1):.j, i:.(j+1), i:.(j-1)]




-- -- given a grid, find the smallest number of steps needed to collect the keys
-- findKeys :: Array2 Char -> Int
-- findKeys grid = explore env initHeap where
--     env = buildEnvironment grid
--     initHeap = (H.singleton (Entry 0 (S.empty, start env)))
-- 
-- 
-- -- in a environment, with a heap of distance (keys, position) either exit if
-- -- we've found all the keys or keep going based on the current options
-- explore :: Env -> Heap HeapEntry -> Int
-- explore env heap 
--     | S.size ks == n = d
--     | otherwise      = explore env heap'
--     where
--         (Entry d (ks, pos)) = H.minimum heap
--         n = M.size (keys env)
--         opts = formatOptions env d ks pos
--         heap' = H.union (H.deleteMin heap) (H.fromList opts)
-- 
-- 
-- formatOptions :: Env -> Int -> Set Char -> Ix2 -> [HeapEntry]
-- formatOptions env d ks pos = map formatter (options env ks pos) where
--     formatter :: (Int, (Char, Ix2)) -> HeapEntry
--     formatter (d', (c, pos')) = Entry (d+d') (S.insert c ks, pos')
-- 
-- 
-- 
-- -- given a set of keys and a position, find the distances to all keys which
-- -- can be reached without passing through locked doors or over other keys
-- options :: Env -> Set Char -> Ix2 -> [(Int, (Char, Ix2))]
-- options env ks pos = go (Sq.singleton (pos, 0)) (S.empty) [] where
--     go :: Seq (Ix2, Int) -> Set Ix2 -> [(Int, (Char, Ix2))] -> [(Int, (Char, Ix2))]
--     go Empty _ found = found
--     go ((p,d):<|qs) seen found
--         | S.member p seen = go qs seen found
--         | isLower c && (not $ S.member c ks) = go qs seen' found'
--         | isLower c = go qs' seen' found
--         | isUpper c && (S.member (toLower c) ks) = go qs' seen' found
--         | c == '.' || c == '@' = go qs' seen' found
--         | otherwise = go qs seen' found
--         where
--             c = tileAt env p
--             qs' = qs >< (Sq.fromList $ zip (neighbours p) (repeat (d+1)))
--             seen' = S.insert p seen
--             found' = found ++ [(d, (c, p))]



-- mains

mainA :: IO ()
mainA = do
    (Just grid) <- parseInput mazeP "18/input.txt"
    let answer = findKeys grid
    print answer
    -- result <- submitAnswer 2019 18 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just grid) <- parseInput mazeP "18/input_b.txt"
    let answer = findKeysB grid
    print answer
    result <- submitAnswer 2019 18 2 answer
    print result
    return ()
