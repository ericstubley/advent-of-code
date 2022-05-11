module Puzzle_2019_14 where

import Automation (submitAnswer)
import Parsing
import Data.Map (Map)
import Data.Set (Set)
import Lens.Micro.Platform
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- data types
data Compound = Compound 
    { _amount :: Int
    , _chemical :: String} 
    deriving (Eq, Ord, Show)
makeLenses ''Compound

type Chemical = String
type Reaction = (Compound, [Compound])
type Materials = Map Chemical Int

type Graph a = Map a (Set a)

-- parsing
reactionsP :: Parser [Reaction]
reactionsP = sepBy reactionP newline


reactionP :: Parser Reaction
reactionP = do
    cs <- sepBy compoundP (string ", ")
    string " => "
    c <- compoundP
    return (c, cs)


compoundP :: Parser Compound
compoundP = do
    n <- natural
    space
    chem <- count' 1 5 upperChar
    return $ Compound n chem 


-- functions
-- topological sort with two helper functions
topologicalSort :: Ord a => Graph a -> [a]
topologicalSort graph
    | M.null graph = []
    | otherwise    = v : (topologicalSort $ removeVertex v graph)
    where v = S.findMin $ findAvailable graph


findAvailable :: Ord a => Graph a -> Set a
findAvailable graph = M.foldl (S.difference) (allVertices) graph where
    allVertices = M.keysSet graph


removeVertex :: Ord a => a -> Graph a -> Graph a
removeVertex v graph = M.delete v graph


-- reaction specific functions
-- you got tripped up because you wrote this backwards
-- if a depends on b this graph has an edge a -> b
-- but really for proper thinking you want a <- b
-- ORE should be a source, FUEL a sink
dependencyGraph :: [Reaction] -> Graph Chemical
dependencyGraph rs = go rs M.empty where
    go [] m = m
    go (r:rs) m = go rs (M.unionWith (S.union) m deps)
      where 
        deps = M.singleton (r ^. (_1 . chemical)) ingredients
        ingredients = S.fromList $ map (\x -> x ^. chemical) (r ^. _2)


recipeLookup :: [Reaction] -> Map Chemical (Int, [Compound])
recipeLookup rs = M.fromList $ map transform rs where
    transform r = (r ^. (_1 . chemical), (r ^. (_1 . amount), r ^. _2))


-- the subtle thing here is that we might have a recipe for m things but need
-- n things, n isn't a multiple of n
-- i.e. if we need 5 but can make in 2s we need to make 3 batches
reqs :: Map Chemical (Int, [Compound]) -> Compound -> Map Chemical Int
reqs recipes compound = M.fromList $ recipe where
    (makes, cps) = recipes M.! (compound ^. chemical)
    batchSize = batch (compound ^. amount) (makes)
    recipe = map (\c -> (c ^. chemical, batchSize * (c ^.amount))) cps


batch :: Int -> Int -> Int
batch desire make = case divMod desire make of
                        (d, 0) -> d
                        (d, _) -> d+1


-- this should be a fold
oreReqFor :: [Reaction] -> Map Chemical Int -> Int
oreReqFor rs want = (go order want) M.! "ORE"
  where
    order = topologicalSort . dependencyGraph $ rs
    recipes = recipeLookup rs
    reqFinder = reqs recipes
    go :: [Chemical] -> Map Chemical Int -> Map Chemical Int
    go [] m = m
    go (c:cs) m
        | M.member c m = go cs m'
        | otherwise    = go cs m
          where 
            cp = Compound (m M.! c) c
            m' = M.unionWith (+) (M.delete c m) (reqFinder cp)


oreReq :: [Reaction] -> Int
oreReq rs = oreReqFor rs (M.singleton "FUEL" 1)


-- binary search
-- find the largest integer x in [lb, ub] where f x is true
-- assuming that f is monotonic false to true
-- no input checking whatsoever....
binarySearch :: (Int -> Bool) -> Int -> Int -> Int
binarySearch f lb ub
    | ub - lb < 2 = if f lb then lb else ub
    | f mid       = binarySearch f mid ub
    | otherwise   = binarySearch f lb mid
    where
        mid = div (ub + lb) 2


viableOreToFuel :: [Reaction] -> Int -> Int -> Bool
viableOreToFuel rs ore fuel = (oreReqFor rs fuelReq) <= ore
    where fuelReq = M.singleton "FUEL" fuel



maxFuel :: [Reaction] -> Int -> Int
maxFuel rs ore = binarySearch (viableOreToFuel rs ore) lb ub
  where
    lb = div ore (oreReq rs)
    ub = 8*lb -- this is a safe upper bound...? but 2 wasn't!



-- mains

mainA :: IO ()
mainA = do
    (Just reactions) <- parseInput reactionsP "14/input.txt"
    let answer = oreReq reactions
    print answer
    -- result <- submitAnswer 2019 14 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just reactions) <- parseInput reactionsP "14/input.txt"
    let answer = maxFuel reactions 1000000000000
    print answer
    result <- submitAnswer 2019 14 2 answer
    print result
    return ()
