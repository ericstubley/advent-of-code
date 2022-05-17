module Puzzle_2019_20 where

import Automation (submitAnswer)
import Parsing
import Grid
import Data.Char (isUpper)
import Data.Map.Strict (Map)
import Data.Sequence (Seq(..), (><), (|>))
import Data.Set (Set)
import qualified Data.Massiv.Array as A
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Sq
import qualified Data.Set as S


-- data types
type Graph = Map Ix2 [Ix2]


-- parsing

donutP :: Parser (Array2 Char)
donutP = many asciiChar >>= (\xs -> return $ A.fromLists' A.Seq (lines xs))


-- functions

neighbours :: Ix2 -> [Ix2]
neighbours (i:.j) = [(i+1):.j, (i-1):.j, i:.(j+1), i:.(j-1)]


buildGraph :: Array2 Char -> Graph
buildGraph donut = A.ifoldlS builder M.empty donut where
    portals = portalMap donut
    builder :: Graph -> Ix2 -> Char -> Graph
    builder graph pos c
        | c == '.' && hasPortal = graph''
        | c == '.'              = graph'
        | otherwise             = graph
        where
            nbhd = neighbours pos
            -- should only be adjance to one portal
            hasPortal = M.member pos portals
            openNbs = filter (\p -> (donut A.! p) == '.') nbhd
            graph' = M.insert pos openNbs graph
            openNbs'' = traversePortal portals pos : openNbs
            graph'' = M.insert pos openNbs'' graph


portalMap :: Array2 Char -> Map Ix2 String
portalMap donut = A.ifoldlS builder M.empty donut where
    builder :: Map Ix2 String -> Ix2 -> Char -> Map Ix2 String
    builder portals (i:.j) c
        | c == '.' && hasPortal = portals'
        | otherwise             = portals
        where
            nbhd = neighbours (i:.j)
            hasPortal = any (\p -> isUpper (donut A.! p)) nbhd
            (i':.j') = head $ filter (\p -> isUpper (donut A.! p)) nbhd
            (di, dj) = (i'-i, j'-j)
            (i'', j'') = (i' + di, j' + dj)
            portal1 = min (i':.j') (i'':.j'')
            portal2 = max (i':.j') (i'':.j'')
            portal = [donut A.! portal1, donut A.! portal2]
            portals' = M.insert (i:.j) portal portals


-- if we start at this portal adjacent index, where do we go?
traversePortal :: Map Ix2 String -> Ix2 -> Ix2
traversePortal portals pos 
    | portal == "AA" || portal == "ZZ" = pos
    | otherwise                        = pos'
    where
        portal = portals M.! pos
        match = M.filterWithKey (\k a -> k /= pos && a == portal) portals
        pos' = head $ M.keys match


terminuses :: Array2 Char -> (Ix2, Ix2)
terminuses donut = (start, end) where
    portals = portalMap donut
    start = fst $ M.findMin $ M.filter (=="AA") portals
    end   = fst $ M.findMin $ M.filter (=="ZZ") portals


alphabetTour :: Array2 Char -> Int
alphabetTour donut = shortestDistance graph start end where
    graph = buildGraph donut
    (start, end) = terminuses donut


shortestDistance :: Graph -> Ix2 -> Ix2 -> Int
shortestDistance graph start end = go initQueue initSeen where
    initQueue = Sq.singleton (start, 0)
    initSeen = S.empty
    go :: Seq (Ix2, Int) -> Set Ix2 -> Int
    go Empty _ = error "Shouldn't get empty queue!"
    go ((pos, d):<|q) seen
        | S.member pos seen = go q seen
        | pos == end        = d
        | otherwise         = go q' seen'
        where
            nbs = graph M.! pos
            newNbs = filter (\x -> not $ S.member x seen) nbs
            q' = q >< (Sq.fromList $ map (\x -> (x, d+1)) newNbs)
            seen' = S.insert pos seen


alphabetTourRecursive :: Array2 Char -> Int
alphabetTourRecursive donut = shortestDistanceRecursive donut start end where
    (start, end) = terminuses donut


shortestDistanceRecursive :: Array2 Char -> Ix2 -> Ix2 -> Int
shortestDistanceRecursive donut start end = go initQueue initSeen where
    initQueue = Sq.singleton (start, 0, 0)
    initSeen = S.empty
    portals = portalMap donut
    go :: Seq (Ix2, Int, Int) -> Set (Ix2, Int) -> Int
    go Empty _ = error "Shouldn't get empty queue!"
    go ((pos, l, d):<|q) seen
        | S.member (pos, l) seen        = go q seen
        | pos == end && l == 0          = d
        | not hasPortal                 = go q' seen'
        -- below are all the having a portal cases
        | portal == "AA" && l /= 0      = go q' seen'
        | portal == "ZZ" && l /= 0      = go q' seen'
        | isOuter donut pos && l == 0   = go q' seen'
        -- finally we can traverse the portal
        | otherwise                     = go q'' seen'
        where
            seen' = S.insert (pos, l) seen
            -- make the list of new neighbours
            -- everything that's another open tile
            -- if there's a portal, can we traverse?
            -- traversal rules: if we're at 0 and outer no
            -- if we're not at 0 then AA and ZZ are closed
            -- if we're at an inner then increase level
            nbhd = neighbours pos
            openNbs = filter (\p -> (donut A.! p) == '.') nbhd
            q' = q >< (Sq.fromList $ map (\x -> (x, l, d+1)) openNbs)
            hasPortal = M.member pos portals
            portal = portals M.! pos
            l' = if isOuter donut pos then l-1 else l+1
            q'' = q' |> (traversePortal portals pos, l', d+1)



-- testing portal positions to see if they're inner or outer
isInner :: Array2 Char -> Ix2 -> Bool
isInner donut pos = not $ isOuter donut pos


isOuter :: Array2 Char -> Ix2 -> Bool
isOuter donut (i:.j) = i == 2 || i == (h-3) || j == 2 || j == (w-3)
    where (A.Sz (h:.w)) = A.size donut


mainA :: IO ()
mainA = do
    (Just donut) <- parseInput donutP "20/input.txt"
    let answer = alphabetTour donut
    print answer
    -- result <- submitAnswer 2019 20 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just donut) <- parseInput donutP "20/input.txt"
    let answer = alphabetTourRecursive donut
    print answer
    -- result <- submitAnswer 2019 20 2 answer
    -- print result
    return ()
