module Puzzle_2018_12 where

import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import System.IO
import Automation (submitAnswer)

type Parser = Parsec Void String
type Rules = Map String Char
type Planters = IntMap Char

-- parsing
parseInput :: String -> IO (Rules, Planters)
parseInput filename = do
    raw <- readFile filename
    let (Right (rules, planters)) = runParser parser "" raw
    return (rules, planters)


pot :: Parser Char
pot = char '.' <|> char '#'

initial :: Parser Planters
initial = do
    string "initial state: "
    potString <- many pot
    let planters = IM.fromList $ zip [0..] potString
    return planters

ruleParser :: Parser (String, Char)
ruleParser = do
    see <- count 5 pot
    string " => "
    result <- pot
    return (see, result)

rulesParser :: Parser Rules
rulesParser = do
    pairs <- ruleParser `sepBy` char '\n'
    let rules = M.fromList pairs
    return rules


parser :: Parser (Rules, Planters)
parser = do
    planters <- initial
    char '\n'
    char '\n'
    rules <- rulesParser
    return (rules, planters)


-- functions


evolve :: Rules -> Planters -> Planters
evolve rules planters = IM.mapWithKey evolver extended where
    evolver :: Int -> Char -> Char
    evolver pos _ = rules M.! (extractPattern planters pos)
    (mi, _) = IM.findMin planters
    (ma, _) = IM.findMax planters
    edges = IM.fromList [(mi-2, '.'), (mi-1, '.'), (ma+1, '.'), (ma+2, '.')]
    extended = IM.union planters edges



boostUp :: Planters -> Planters
boostUp planters = IM.unions (planters : nbhds) where
    nbhds = map (\x -> nbhd (fst x)) $ IM.toList planters
    nbhd p = IM.fromList [(p-2, '.'), (p-1, '.'), (p+1, '.'), (p+2, '.')]


stripDown :: Planters -> Planters
stripDown planters = IM.filter (== '#') planters


smartEvolve :: Rules -> Planters -> Planters
smartEvolve rules planters = stripDown . (evolve rules) . boostUp $ planters




extractPattern :: Planters -> Int -> String
extractPattern planters pos = map (\p -> find p) [(pos-2)..(pos+2)] where
    find p
        | IM.notMember p planters   = '.'
        | otherwise                 = planters IM.! p


indexSum :: Planters -> Int
indexSum planters = IM.foldlWithKey (\acc k _ -> acc + k) 0 withPlants where
    withPlants = IM.filter (== '#') planters





mainA :: IO ()
mainA = do
    (rules, planters) <- parseInput "input.txt"
    let answer = indexSum $ (iterate (smartEvolve rules) planters) !! 20
    print answer
    -- result <- submitAnswer 2018 12 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (rules, planters) <- parseInput "input.txt"
    let a1 = indexSum $ (iterate (smartEvolve rules) planters) !! 1000
    let a2 = indexSum $ (iterate (smartEvolve rules) planters) !! 2000
    -- let a3 = indexSum $ (iterate (smartEvolve rules) planters) !! 3000
    -- let a4 = indexSum $ (iterate (smartEvolve rules) planters) !! 4000
    let oneKSteps = a2 - a1
    let answer = ((50000000000 `div` 1000) - 1) * oneKSteps + a1
    print answer
    result <- submitAnswer 2018 12 2 answer
    print result
    return ()
