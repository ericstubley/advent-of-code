module Puzzle_2018_07 where

import Data.Char (ord)
import Data.Heap (Heap)
import qualified Data.Heap as Heap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import System.IO
import Automation (submitAnswer)


-- data types
type Parser = Parsec Void String

type Graph = Map Char (Set Char)

type Edge = (Char, Char)

-- parsing

parseInput :: String -> IO Graph
parseInput filename = do
    raw <- readFile filename
    let (Right edges) = runParser linesParser "" raw
    let graph = graphFromEdges edges
    return graph


linesParser :: Parser [Edge]
linesParser = lineParser `sepBy` char '\n'


lineParser :: Parser Edge
lineParser = do
   string "Step "
   start <- upperChar
   string " must be finished before step " 
   end <- upperChar
   string " can begin."
   return (start, end)


graphFromEdges :: [Edge] -> Graph
graphFromEdges edges = foldl inserter (Map.empty) edges where
    inserter graph (start, end) = Map.insertWith (Set.union) end (Set.empty) $ Map.insertWith (Set.union) start (Set.singleton end) graph


-- functions
topologicalSort :: Graph -> [Char]
topologicalSort graph
    | graph == Map.empty    = []
    | otherwise             = v : (topologicalSort $ removeVertex v graph)
    where
        v = Set.findMin $ findAvailable graph


findAvailable :: Graph -> Set Char
findAvailable graph = Map.foldl (Set.difference) (allKeys) graph where
    allKeys = Set.fromList . (map fst) $ Map.toList graph


removeVertex :: Char -> Graph -> Graph
removeVertex v graph = Map.delete v graph


timeToCompletion :: Int -> Int -> Graph -> Int
timeToCompletion workers buffer graph = timeHelper Heap.empty graph where
    timeHelper :: Heap (Int, Char) -> Graph -> Int
    timeHelper heap graph
        | graph == Map.empty    = 0
        | canDropFromHeap       = timeHelper (Heap.drop 1 heap) (removeVertex dropped graph)
        | canAddToHeap          = timeHelper addedHeap (Map.adjust (Set.insert added) added graph)
        | otherwise             = m + timeHelper (Heap.map (\x -> (fst x - m, snd x)) heap) graph
        where
            canDropFromHeap = (heap /= Heap.empty) && fst (Heap.minimum heap) == 0
            dropped = snd (Heap.minimum heap)
            canAddToHeap = Heap.size heap < workers && (Set.size $ findAvailable graph) > 0
            added = Set.findMin $ findAvailable graph
            addedHeap = Heap.insert (buffer + taskTime added, added) heap
            m = fst (Heap.minimum heap)


taskTime :: Char -> Int
taskTime c = ord c - 64

-- main functions

mainA :: IO ()
mainA = do
    graph <- parseInput "input.txt"
    let answer = topologicalSort graph
    print answer
    -- result <- submitAnswer 2018 07 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    graph <- parseInput "input.txt"
    let answer = timeToCompletion 5 60 graph
    print answer
    result <- submitAnswer 2018 07 2 answer
    print result
    return ()
