module Puzzle_2019_06 where

import Automation (submitAnswer)
import Parsing
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- data types
type Orrery = Map String String



-- parsing
orbitsP :: Parser Orrery
orbitsP = do
    pairs <- sepBy orbitP newline
    return $ M.fromList pairs


orbitP :: Parser (String, String)
orbitP = do
    center <- objectP
    char ')'
    orbiter <- objectP
    return (orbiter, center)


objectP :: Parser String
objectP = count' 1 3 (upperChar <|> digitChar)


-- functions


orbitSum :: Orrery -> Int
orbitSum orrery = sum . (map (depth orrery)) $ M.keys orrery


depth :: Orrery -> String -> Int
depth orrery object = go object 0 where
    go :: String -> Int -> Int
    go object d
        | object == center = d
        | otherwise        = go (orrery M.! object) (d+1)


decay :: Orrery -> String -> [String]
decay orrery object
    | object == center = []
    | otherwise        = parent : decay orrery parent
    where parent = orrery M.! object


transfers :: Orrery -> String -> String -> Int
transfers orrery start end = S.size transferPath
  where
    sd = S.fromList $ decay orrery start
    ed = S.fromList $ decay orrery end
    bothPaths = S.union sd ed
    stemPath = S.intersection sd ed
    transferPath = S.difference bothPaths stemPath


center :: String
center = "COM"


you :: String
you = "YOU"


santa :: String
santa = "SAN"


-- mains

mainA :: IO ()
mainA = do
    (Just orrery) <- parseInput orbitsP "06/input.txt"
    let answer = orbitSum orrery
    print answer
    -- result <- submitAnswer 2019 06 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just orrery) <- parseInput orbitsP "06/input.txt"
    let answer = transfers orrery you santa
    print answer
    result <- submitAnswer 2019 06 2 answer
    print result
    return ()
