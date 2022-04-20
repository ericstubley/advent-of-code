module Puzzle_2018_20 where


import Data.Bits ((.&.), (.|.))
import Data.Sequence (Seq(..), (><), (<|))
import Data.Map.Strict (Map)
import Data.Massiv.Array (Ix2(..))
import qualified Data.Map.Strict as M
import qualified Data.Massiv.Array as A
import qualified Data.Sequence as S
import Numeric (showHex)
import System.IO
import Automation (submitAnswer)

-- data types

type BaseMap = Map Ix2 Int
type BFS = Map Ix2 Int

-- laying it out in comments here
-- a 4 bit int will contain the door info for a room
-- in positions WSEN
-- so 12 means a room with doors to S and W, 15 means all 4, 0 means none


-- parsing

parseInput :: String -> IO String
parseInput filename = readFile filename


-- functions

graphFromRegex :: String -> BaseMap
graphFromRegex r = snd $ graphHelper r (S.singleton (0:.0)) (M.singleton (0:.0) 0) where
    -- keep a stack of positions in the Seq
    graphHelper :: String -> Seq Ix2 -> BaseMap -> (String, BaseMap)
    graphHelper (c:rs) (p:<|ps) bm = case c of
        '^' -> graphHelper rs (p<|ps) bm
        '$' -> ("", bm)
        '(' -> graphHelper rs (p<|p<|ps) bm -- push to stack and keep going
        ')' -> graphHelper rs ps bm -- pop off stack and keep going
        '|' -> graphHelper rs (h<|ps) bm -- refresh from stack and keep going
        _   -> graphHelper rs (p'<|ps) bm'
        where
            h = S.index ps 0
            (p', bm') = addDoor bm p c





addDoor :: BaseMap -> Ix2 -> Char -> (Ix2, BaseMap)
addDoor bm (i:.j) d = (p', M.unionWith ((.|.)) bm updates) where
    p' = case d of
        'N' -> (i-1):.j
        'E' -> i:.(j+1)
        'S' -> (i+1):.j
        'W' -> i:.(j-1)
    updates = case d of
        'N' -> M.fromList [(i:.j, 1), ((i-1):.j, 4)]
        'E' -> M.fromList [(i:.j, 2), (i:.(j+1), 8)]
        'S' -> M.fromList [(i:.j, 4), ((i+1):.j, 1)]
        'W' -> M.fromList [(i:.j, 8), (i:.(j-1), 2)]



neighbours :: Ix2 -> Int -> [Ix2]
neighbours (i:.j) doors = map snd $ filter doorCheck nbs where
    nbs = zip [0..] [(i-1):.j, i:.(j+1), (i+1):.j, i:.(j-1)]
    doorCheck :: (Int, Ix2) -> Bool
    doorCheck (d, _) = ((2^d) .&. doors) /= 0


bfs :: BaseMap -> Ix2 -> BFS
bfs bm start = bfsHelper (S.singleton (start, 0)) M.empty where
    bfsHelper :: Seq (Ix2, Int) -> BFS -> BFS
    bfsHelper Empty searched = searched
    bfsHelper ((ix, d):<|qs) searched
        | M.member ix searched  = bfsHelper qs searched
        | otherwise             = bfsHelper (qs >< nbs) (M.insert ix d searched)
        where nbs = S.fromList $ zip (neighbours ix (bm M.! ix)) (repeat (d+1))


printableMap :: BaseMap -> [String]
printableMap bm = map mapLine [top..bottom] where
        keys = map A.fromIx2 $ M.keys bm
        left = minimum $ map snd $ keys
        right = maximum $ map snd $ keys
        top = minimum $ map fst $ keys
        bottom = maximum $ map fst $ keys
        mapLine :: Int -> String
        mapLine x = concat $ map (\y -> showHex (M.findWithDefault 0 (x:.y) bm) "") [left..right]



printMap :: BaseMap -> IO ()
printMap bm = putStrLn "" >> mapM_ putStrLn (printableMap bm)


furthestRoom :: String -> Int
furthestRoom r = maximum dists where
    g = graphFromRegex r
    dists = M.elems $ bfs g (0:.0)


countFarRooms :: String -> Int
countFarRooms r = M.size $ M.filter (>= 1000) dists where
    g = graphFromRegex r
    dists = bfs g (0:.0)

-- mains

mainA :: IO ()
mainA = do
    r <- parseInput "input.txt"
    let answer = furthestRoom r
    print answer
    -- result <- submitAnswer 2018 20 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    r <- parseInput "input.txt"
    let answer = countFarRooms r
    print answer
    result <- submitAnswer 2018 20 2 answer
    print result
    return ()
