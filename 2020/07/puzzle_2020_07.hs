module Puzzle_2020_07 where

import Automation (submitAnswer)
import Parsing
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- data types
data Bag = Bag {adjective :: String, colour :: String} 
            deriving (Eq, Show, Ord)


type Rules = [(Bag, [(Int, Bag)])]
type Graph = Map Bag [Bag]

-- parsing
graphP :: Parser Graph
graphP = rulesToGraph <$> rulesP

rulesP :: Parser Rules
-- rulesP = M.fromList <$> sepBy lineP newline
rulesP = sepBy lineP newline

lineP :: Parser (Bag, [(Int, Bag)])
lineP = do
   bag <- bagP
   string " contain " 
   contained <- containedP
   char '.'
   return (bag, contained)

bagP :: Parser Bag
bagP = do
    adj <- many lowerChar
    char ' '
    col <- many lowerChar
    string " bag"
    optional $ char 's'
    return $ Bag adj col

containedP :: Parser [(Int, Bag)]
containedP = emptyBagP <|> some containedBagP

emptyBagP :: Parser [(Int, Bag)]
emptyBagP = try (string "no other bags") >> return []

containedBagP :: Parser (Int, Bag)
containedBagP = do
    amount <- integer
    char ' '
    bag <- bagP
    optional $ string ", "
    return (amount, bag)

-- functions
rulesToGraph :: Rules -> Graph
rulesToGraph [] = M.empty
rulesToGraph ((bag, contained):rs) = M.unionWith (++) arrows (rulesToGraph rs)
  where bags' = map snd contained
        arrows = M.fromList $ zip bags' (repeat [bag])


forwardSize :: Graph -> Bag -> Int
forwardSize graph bag = S.size $ helper bag
  where helper :: Bag -> Set Bag
        helper bag = S.unions $ [S.singleton bag] ++ sets
          where bags' = M.findWithDefault [] bag graph
                sets = map helper bags'


containers :: Graph -> Bag -> Int
containers graph bag = (forwardSize graph bag) - 1


bagsContained :: Rules -> Bag -> Int
bagsContained rules bag = counter bag
  where rules' = M.fromList rules
        mathHelp :: (Int, Bag) -> Int
        mathHelp (n, b) = n*(1 + (counter b))
        counter :: Bag -> Int
        counter bag
            | M.member bag rules' = sum $ map mathHelp (rules' M.! bag)
            | otherwise = 0

-- main

mainA :: IO ()
mainA = do
    (Just graph) <- parseInput graphP "07/input.txt"
    let answer = containers graph (Bag "shiny" "gold")
    print answer
    -- result <- submitAnswer 2020 07 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just rules) <- parseInput rulesP "07/input.txt"
    let answer = bagsContained rules (Bag "shiny" "gold")
    print answer
    -- result <- submitAnswer 2020 07 2 answer
    -- print result
    return ()
