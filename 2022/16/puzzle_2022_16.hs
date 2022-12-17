module Puzzle_2022_16 where

import Automation (submitAnswer)
import Parsing
import Data.Map.Strict (Map)
import Data.Maybe (Maybe(..))
import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.RWS
import Data.Bits

-- data types
type Adjacency a = Map (a, a) Int

data Search = Search
    { _time :: Int
    , _location :: Int
    , _valves :: Int} deriving (Eq, Ord, Show)


data Searcher = Searcher
    { _time' :: Int
    , _distance1 :: Int
    , _distance2 :: Int
    , _location1 :: Int
    , _location2 :: Int
    , _valves' :: Int} deriving (Eq, Ord, Show)

type Memo = Map Search Int
type Memo2 = Map Searcher Int

data Graph = Graph
    { _adjacency :: Adjacency Int
    , _rates :: Map Int Int} deriving (Eq, Ord, Show)

-- parsing
graphP :: Parser (Adjacency String, Map String Int)
graphP = do
    xs <- sepBy nodeP newline
    let as = map fst xs
    let rs = map snd xs
    return (M.unions as, M.unions rs)


-- Valve NV has flow rate=5; tunnels lead to valves ZV, CG, YB, HX, OY
nodeP :: Parser (Adjacency String, Map String Int)
nodeP = do
    string "Valve "
    name <- nameP
    string " has flow rate="
    rate <- natural
    string "; tunnels lead to valves " <|> string "; tunnel leads to valve "
    tunnels <- sepBy nameP (string ", ")
    let edges = zip (repeat name) tunnels
    return (M.fromList $ zip edges (repeat 1), M.singleton name rate)


nameP :: Parser String
nameP = count 2 upperChar


-- functions
-- collapse all the useless edges
-- when removing a useless node, you need to
--  delete the node
--  for the two neighbour nodes, change the edges + increase length
simplify :: Map String Int -> Adjacency String -> Adjacency String
simplify rates adjacencies = M.foldrWithKey' updater adjacencies rates where
    updater :: String -> Int -> Adjacency String -> Adjacency String
    updater ident rate as
        | ident /= "AA" && rate == 0 = as'
        | otherwise                  = as
          where edges = M.filterWithKey (\k a -> fst k == ident) as
                reverseEdges = M.filterWithKey (\k a -> snd k == ident) as
                ((_, v0), d0) = M.elemAt 0 edges
                ((_, v1), d1) = M.elemAt 1 edges
                removed = M.difference as (M.union edges reverseEdges)
                as' = M.union removed 
                        $ M.fromList [((v0, v1), d0+d1), ((v1, v0), d1+d0)]


renamer :: Map String Int -> (String -> Int)
renamer rates = (\n -> mapping M.! n) where
    mapping = M.fromList $ zip (M.keys rates) [0..]


preprocess :: Adjacency String -> Map String Int -> Graph
preprocess as rates = Graph as' rates' where
    namer = renamer rates
    rates' = M.mapKeys namer rates
    as' = M.mapKeys (both namer) $ simplify rates as


both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)


maximumRelease :: Adjacency String -> Map String Int -> Int
maximumRelease as rates = fst $ evalRWS (maximize initial) graph M.empty
  where graph = preprocess as rates
        initial = Search 0 0 0



-- memoized search through paths
maximize :: MonadRWS Graph () Memo m 
            => Search -> m Int
maximize (Search t l v) = do
    cached <- gets (M.lookup (Search t l v))
    case (cached, t >= 30) of
        (Just x, _)      -> return x
        (Nothing, True)  -> return 0
        (Nothing, False) -> do
            release <- letoff v
            -- turn on the current bit
            toggle <- maximize (Search (t+1) l (v .|. (bit l)))
            let toggle' = release + toggle
            -- explore all the pathways
            -- extract from the reader instance all the paths as a list of
            --  (location, distance) pairs
            let pathFinder = \k a -> fst k == l
            options <- reader (M.toList . M.mapKeys snd . M.filterWithKey pathFinder . _adjacency)
            -- compute the maximizations along those paths
            let searches = map (\x -> Search (t + snd x) (fst x) v) options
            paths <- mapM maximize searches
            -- add on multiples of release
            -- be careful to only add min (d, (30 - t)) times release
            let multiplier = min (30-t)
            let dists = map snd options
            let releases = map (* release) $ map multiplier dists
            let paths' = zipWith (+) paths releases
            -- pick the global maximum and cache it
            let optimum = maximum (toggle' : paths')
            modify $ M.insert (Search t l v) optimum
            return optimum



-- given the current set of open valves, compute how much pressure is let off
letoff :: MonadReader Graph m 
       => Int -> m Int
letoff valves = do
    let f = \k a -> testBit valves k
    reader (sum . M.elems . M.filterWithKey f . _rates)


collaborativeRelease :: Adjacency String -> Map String Int -> Int
collaborativeRelease as rates = fst $ evalRWS (collaborate initial) graph M.empty
  where graph = preprocess as rates
        initial = Searcher 0 0 0 0 0 0


-- in principle this could work but its hella slow
-- memoized search through paths
collaborate :: MonadRWS Graph () Memo2 m 
            => Searcher -> m Int
collaborate (Searcher t d1 d2 l1 l2 v) = do
    cached <- gets (M.lookup (Searcher t d1 d2 l1 l2 v))
    case (cached, t >= 26) of
        (Just x, _)      -> return x
        (Nothing, True)  -> return 0
        (Nothing, False) -> do
            release <- letoff v
            case (d1 == 0, d2 == 0) of
                (False, False) -> do
                    value <- collaborate (Searcher (t+1) (d1-1) (d2-1) l1 l2 v)
                    modify $ M.insert (Searcher t d1 d2 l1 l2 v) (release + value)
                    return (release + value)
                (False, True)  -> collaborate (Searcher t d2 d1 l2 l1 v)
                (True, False)  -> do
                    toggle <- collaborate (Searcher (t+1) 0 (d2-1) l1 l2 (v .|. (bit l1)))

                    -- compute the possible paths agent 1 could take
                    let pathFinder = \k a -> fst k == l1
                    options <- reader (M.toList . M.mapKeys snd . M.filterWithKey pathFinder . _adjacency)
                    -- compute the maximizations along those paths
                    let searches = map (\x -> Searcher (t + 1) ((-) 1 (snd x)) (d2 - 1) (fst x) d2 v) options
                    paths <- mapM collaborate searches

                    let optimum = release + maximum (toggle : paths)
                    modify $ M.insert (Searcher t d1 d2 l1 l2 v) optimum
                    return optimum
                (True, True)   -> do
                    let pathFinder1 = \k a -> fst k == l1
                    options1 <- reader (M.toList . M.mapKeys snd . M.filterWithKey pathFinder1 . _adjacency)
                    let pathFinder2 = \k a -> fst k == l2
                    options2 <- reader (M.toList . M.mapKeys snd . M.filterWithKey pathFinder2 . _adjacency)

                    let tree = [(o1, o2) | o1 <- ((l1, 0) : options1)
                                         , o2 <- ((l2, 0) : options2)
                                         , o1 /= o2]
                    let searches = map (buildSearcher (Searcher t d1 d2 l1 l2 v)) tree
                    paths <- mapM collaborate searches
                    let optimum = maximum paths

                    modify $ M.insert (Searcher t d1 d2 l1 l2 v) optimum
                    return optimum


buildSearcher :: Searcher -> ((Int, Int), (Int, Int)) -> Searcher
buildSearcher (Searcher t _ _ l1 l2 v) ((v1, d1), (v2, d2))
    | l1 == v1 && l2 == v2 = Searcher (t-1) 0 0 l1 l2 (v .|. (bit l1) .|. (bit l2))
    | l1 == v1 && l2 /= v2 = Searcher (t-1) 0 (d2-1) l1 v2 (v .|. (bit l1))
    | l1 /= v1 && l2 == v2 = Searcher (t-1) (d1-1) 0 v1 l2 (v .|. (bit l2))
    | l1 /= v1 && l2 /= v2 = Searcher (t-1) (d1-1) (d2-1) v1 v2 v



-- mains

mainA :: IO ()
mainA = do
    (Just (adjacencies, rates)) <- parseInput graphP "16/input.txt"
    let answer = maximumRelease adjacencies rates
    print answer
    -- result <- submitAnswer 2022 16 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    let answer = 0
    print answer
    -- result <- submitAnswer 2022 16 2 answer
    -- print result
    return ()
