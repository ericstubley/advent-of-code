module Puzzle_2022_19 where

import Automation (submitAnswer)
import Parsing
import Lens.Micro.Platform
import Linear.V4
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Applicative (liftA2)

-- data types
data Resource = Ore | Clay | Obsidian | Geode 
    deriving (Eq, Ord, Show, Enum, Bounded)

type Vec = V4 Int

type Blueprint = Map Resource Vec

-- parsing
blueprintsP :: Parser [Blueprint]
blueprintsP = sepBy blueprintP newline


blueprintP :: Parser Blueprint
blueprintP = do
    string "Blueprint "
    natural
    char ':'
    costs <- count 4 costP
    return $ M.fromList $ zip [minBound :: Resource .. ] costs


costP :: Parser Vec
costP = do
    string " Each "
    resourceP
    string " robot costs "
    amounts <- sepBy amountP (string " and ")
    char '.'
    return $ sum amounts


amountP :: Parser Vec
amountP = do
    n <- natural
    char ' '
    v <- rVec <$> resourceP 
    return (fmap ((*) n) v)


resourceP :: Parser Resource
resourceP = (string "ore" >> return Ore)
        <|> (string "clay" >> return Clay)
        <|> (string "obsidian" >> return Obsidian)
        <|> (string "geode" >> return Geode)


rVec :: Resource -> Vec
rVec r = case r of
            Ore      -> V4 1 0 0 0
            Clay     -> V4 0 1 0 0
            Obsidian -> V4 0 0 1 0
            Geode    -> V4 0 0 0 1


-- rLens :: Resource -> Lens' Vec Int
rLens r = case r of
            Ore      -> _x
            Clay     -> _y
            Obsidian -> _z
            Geode    -> _w


-- functions
qualitySum :: [Blueprint] -> Int -> Int
qualitySum bps limit = sum . zipWith (*) [1..] $ map (\b -> maximizeGeodes b limit) bps


geodeProduct :: [Blueprint] -> Int -> Int
geodeProduct bps limit = product . map (\b -> maximizeGeodes b limit) $ take 3 bps


blueprintMax :: Blueprint -> Resource -> Int
blueprintMax bp Geode = maxBound :: Int
blueprintMax bp r = maximum . map (^. (rLens r)) . M.elems $ bp


openBuild :: V4 Bool
openBuild = V4 True True True True


resources :: [Resource]
resources = [Ore, Clay, Obsidian, Geode]


maximizeGeodes :: Blueprint -> Int -> Int
maximizeGeodes bp limit = go 1 (V4 1 0 0 0) (V4 0 0 0 0) openBuild 
  where
    enoughMaterials :: Vec -> Resource -> Bool
    enoughMaterials materials r = all (>= 0) (materials - (bp M.! r))

    canBuild :: V4 Bool -> Resource -> Bool
    canBuild flags r = flags ^. (rLens r)

    notSuperfluous :: Vec -> Resource -> Bool
    notSuperfluous bots r = (bots ^. (rLens r)) < (blueprintMax bp r)

    go :: Int -> Vec -> Vec -> V4 Bool -> Int
    go t bots materials flags
        | t == limit          = (bots + materials) ^. (rLens Geode)
        | length options == 4 = maximum paths
        | otherwise           = maximum (noBuild : paths)
          where f = \r -> (canBuild flags r) 
                          && (enoughMaterials materials r)
                          && (notSuperfluous bots r)
                options = filter f resources
                paths = map (path t bots materials flags) options
                flags' = fmap not (V4 (f Ore) (f Clay) (f Obsidian) (f Geode))
                flags'' = liftA2 (&&) flags flags'
                noBuild = go (t+1) bots (materials + bots) flags''

    path :: Int -> Vec -> Vec -> V4 Bool -> Resource -> Int
    path t bots materials flags r = go (t+1) bots' materials' openBuild
      where bots' = bots + (rVec r)
            materials' = materials + bots - (bp M.! r)

-- filter by:
--  flag says can build
--  have enough material to build
--  are under blueprint max
-- if you could build any bot, then you should always build a bot

-- when might we not build a bot?
--  because we are saving resources for a bot we can't currently build
--  in this case, search the path where you don't build a bot and aren't
--      allowed to build the currently allowable bots
--  so flags' is flags && (True if not in options, False if in options)

-- mains

mainA :: IO ()
mainA = do
    (Just blueprints) <- parseInput blueprintsP "19/input.txt"
    let answer = qualitySum blueprints 24
    print answer
    -- result <- submitAnswer 2022 19 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just blueprints) <- parseInput blueprintsP "19/input.txt"
    let answer = geodeProduct blueprints 32
    print answer
    result <- submitAnswer 2022 19 2 answer
    print result
    return ()
