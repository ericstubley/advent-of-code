module Puzzle_2023_25 where

import Automation (submitAnswer)
import Parsing

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query
import Data.GraphViz
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Sort (sort, sortOn)

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL

-- data types


-- parsing
wiresP :: Parser (Set (String, String))
wiresP = buildSet <$> sepBy wireP newline

wireP :: Parser (String, [String])
wireP = do
    v <- nameP
    string ": "
    vs <- sepBy nameP (char ' ')
    return (v, vs)

nameP :: Parser String
nameP = count 3 letterChar

buildSet :: [(String, [String])] -> Set (String, String)
buildSet ls = S.unions . map (S.fromList . builder) $ ls
  where builder (s, []) = []
        builder (s, k:ks) = (s,k):(k,s): builder (s, ks)

-- functions
degrees :: Set (String, String) -> [(String, Int)]
degrees graph = sortOn snd $ go processed
  where processed = sort . map fst . S.toList $ graph
        go [] = []
        go ks@(k:_) = (k, length . takeWhile (==k) $ ks) : go (dropWhile (==k) ks)


-- buildMap :: Set (String, String) -> Map String [String]
-- buildMap = M.unionsWith (++) . map (\t -> M.singleton (fst t) [snd t]) 


buildGraph :: Set (String, String) -> Gr String String
buildGraph edgeSet = mkGraph nodes edges
  where ns = S.toList . S.map fst $ edgeSet
        mapping = M.fromList $ zip ns [0..]
        nodes = map (\t -> (snd t, fst t)) . M.toList $ mapping
        edges = map (\t -> (mapping M.! (fst t), mapping M.! (snd t), "")) . S.toList $ edgeSet

defaultVis :: Gr String String -> DotGraph Node
defaultVis = graphToDot quickParams


-- mains

mainA :: IO ()
mainA = do
    (Just es) <- parseInput wiresP "25/input.txt"
    let answer = TL.toStrict . printDotGraph . defaultVis . buildGraph $ es
    T.writeFile "dot_25.txt" answer
    let removals = S.fromList [ ("vgf", "jpn")
                              , ("jpn", "vgf")
                              , ("fdb", "txm")
                              , ("txm", "fdb")
                              , ("nmz", "mnl")
                              , ("mnl", "nmz") ]
    let es' = S.difference es removals
    print $ length es
    print $ length es'
    let cs = components $ buildGraph es'
    print $ length cs
    print $ (map length) cs

    -- print answer
    -- result <- submitAnswer 2023 25 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    let answer = 0
    print answer
    -- result <- submitAnswer 2023 25 2 answer
    -- print result
    return ()
