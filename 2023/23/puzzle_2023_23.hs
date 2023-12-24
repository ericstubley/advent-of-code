module Puzzle_2023_23 where

import Automation (submitAnswer)
import Parsing
import Search (dfs)

import Control.Monad.Reader
import Data.Map.Strict (Map)
import Data.Massiv.Array (Array, Ix2(..), Sz(..))
import Data.Maybe
import Data.Sequence (Seq(..), (><))
import Data.Set (Set)

import qualified Data.Map.Strict as M
import qualified Data.Massiv.Array as A
import qualified Data.Sequence as Sq
import qualified Data.Set as S

-- data types
type Array2 a = A.Array A.U A.Ix2 a
type Mountain = Array2 Char

type Path = (Ix2, Set Ix2)

type Graph = Map Ix2 [(Ix2, Int)]

-- parsing
mountainP :: Parser Mountain
mountainP = A.fromLists' A.Seq <$> sepBy (some squareP) newline

squareP :: Parser Char
squareP = choice $ map char ".#<>^v"

-- functions
initial :: Path
initial = (0 :. 1, S.singleton (0:.1))


terminator :: MonadReader Mountain m => Path -> m (Maybe Int)
terminator ((i:.j), path) = do
    s <- reader A.size
    let (A.Sz (h:.w)) = s
    if i==h-1 && j == w-2
        then return (Just 0)
        else return Nothing


-- don't bother checking grid directions
-- if on an arrow only option is to follow it
-- candidates are the 4-neighbours

generator :: MonadReader Mountain m => Path -> m [Path]
generator (ix, path) = do
    c <- reader $ \mtn -> mtn A.! ix
    let candidates = if elem c "^>v<" then [follow c ix] else (ns ix)
    let candidates' = filter (\x -> not $ S.member x path) candidates
    candidates'' <- filterM inBounds candidates' >>= filterM walkable
    return $ map (\x -> (x, S.insert x path)) candidates''

follow :: Char -> Ix2 -> Ix2
follow c (i:.j)
    | c == '^' = (i-1) :. j
    | c == '>' = i :. (j+1)
    | c == 'v' = (i+1) :. j
    | c == '<' = i :. (j-1)

ns :: Ix2 -> [Ix2]
ns (i:.j) = [ (i+1) :. j
            , (i-1) :. j
            , i :. (j+1)
            , i :. (j-1)]

inBounds :: MonadReader Mountain m => Ix2 -> m Bool
inBounds ix = do
    s <- reader A.size
    return $ A.isSafeIndex s ix

walkable :: MonadReader Mountain m => Ix2 -> m Bool
walkable ix = do
    c <- reader (\mtn -> mtn A.! ix)
    return (c /= '#')

computer :: MonadReader Mountain m => Path -> [Int] -> m Int
computer _ [] = return flag
computer _ paths
    | best == flag = return flag
    | otherwise    = return (1 + best)
      where best = maximum paths


flag :: Int
flag = minBound :: Int


longestPath :: Mountain -> Int
longestPath mountain = runReader (dfs initial terminator generator computer) mountain


-- part B
-- generatorB :: MonadReader Mountain m => Path -> m [Path]
-- generatorB (ix, path) = do
--     c <- reader $ \mtn -> mtn A.! ix
--     let candidates = ns ix
--     let candidates' = filter (\x -> not $ S.member x path) candidates
--     candidates'' <- filterM inBounds candidates' >>= filterM walkable
--     return $ map (\x -> (x, S.insert x path)) candidates''


-- longestPathB :: Mountain -> Int
-- longestPathB mountain = runReader (dfs initial terminator generatorB computer) mountain

-- the above are incredibly slow
-- let's work out the junctures

buildGraph :: Mountain -> Graph
buildGraph mountain = runReader (explorer >>= collapse) mountain

explorer :: MonadReader Mountain m => m (Map Ix2 [Ix2])
explorer = go M.empty (Sq.singleton (0:.1))
  where go graph Sq.Empty = return graph
        go graph (a :<| queue)
            | M.member a graph = go graph queue
            | otherwise        = do
                edges <- neighbours a
                go (M.insert a edges graph) (queue >< Sq.fromList edges)

neighbours :: MonadReader Mountain m => Ix2 -> m [Ix2]
neighbours ix = do
    let candidates = ns ix
    filterM inBounds candidates >>= filterM walkable

collapse :: MonadReader Mountain m => Map Ix2 [Ix2] -> m Graph
collapse graph = do
    s <- reader A.size
    let (A.Sz (h:.w)) = s
    let checkpoints = junctures graph ++ [0:.1, (h-1):.(w-2)]
    return $ M.fromList $ zip checkpoints (map (dists graph checkpoints) checkpoints)

junctures :: Map Ix2 [Ix2] -> [Ix2]
junctures = M.keys . M.filter (\es -> length es > 2) 

dists :: Map Ix2 [Ix2] -> [Ix2] -> Ix2 -> [(Ix2, Int)]
dists graph checkpoints v = map walker input
  where input = map (\x -> x:[v]) $ graph M.! v
        walker (h:p)
            | elem h checkpoints = (h, length p) 
            | otherwise          = walker (h':h:p)
              where h' = head . filter (\x -> not $ elem x p) $ graph M.! h


longestPathB :: Mountain -> Int
longestPathB mountain = d
  where graph = buildGraph mountain
        path = runReader (dfs initial terminatorB generatorB computerB) graph
        d = runReader (survey path) graph


end :: MonadReader Graph m => m Ix2
end = do
    vs <- reader $ M.keys . M.filter (\es -> length es == 1)
    return $ head . filter (\v -> v /= (0:.1)) $ vs

terminatorB :: MonadReader Graph m => Path -> m (Maybe [Ix2])
terminatorB (v, _) = do
    vFinal <- end
    if v == vFinal
        then do
            es <- reader $ \g -> g M.! vFinal
            return $ Just [vFinal]
        else return Nothing


generatorB :: MonadReader Graph m => Path -> m [Path]
generatorB (v, path) = do
    es <- reader $ \g -> g M.! v
    let candidates = map fst es
    let candidates' = filter (\x -> not $ S.member x path) candidates
    return $ map (\x -> (x, S.insert x path)) candidates'

computerB :: MonadReader Graph m => Path -> [[Ix2]] -> m [Ix2]
computerB _ [] = return []
computerB (v, _) paths = do
    let paths' = map (\x -> if x == [] then [] else (v:x)) paths
    lengths <- mapM survey paths'
    return $ snd . maximum $ zip lengths paths'


survey :: MonadReader Graph m => [Ix2] -> m Int
survey [] = return 0
survey (x:[]) = return 0
survey (v:w:es) = do
    ves <- reader $ \g -> g M.! v
    let e = snd . head . filter (\x -> fst x == w) $ ves
    l <- survey (w:es)
    return (e+l)


-- mains

mainA :: IO ()
mainA = do
    (Just mountain) <- parseInput mountainP "23/input.txt"
    let answer = longestPath mountain
    print answer
    -- result <- submitAnswer 2023 23 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just mountain) <- parseInput mountainP "23/input.txt"
    -- let graph = buildGraph mountain
    -- mapM print $ M.toList graph
    let answer = longestPathB mountain
    print answer
    -- result <- submitAnswer 2023 23 2 answer
    -- print result
    return ()
