module Puzzle_2023_08 where

import Automation (submitAnswer)
import Parsing
import Data.Map.Strict (Map)
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Applicative (liftA2)

import qualified Data.Map.Strict as M

-- data types
data Direction = L | R deriving (Eq, Ord, Show)

data Environment = Environment { _nodes :: Map (String, Direction) String
                               , _instructions :: [Direction]
                               , _period :: Int }
                                 deriving (Eq, Ord, Show)

-- parsing
inputP :: Parser Environment
inputP = do
    instructions <- many directionP
    newline
    newline
    nodes <- sepBy nodeP newline
    return $ Environment (M.unions nodes) instructions (length instructions)

directionP :: Parser Direction
directionP = (char 'L' >> return L) <|> (char 'R' >> return R)

nodeP :: Parser (Map (String, Direction) String)
nodeP = do
    a <- nameP
    string " = ("
    l <- nameP
    string ", "
    r <- nameP
    char ')'
    return $ M.fromList [((a, L), l), ((a, R), r)]

nameP :: Parser String
nameP = count 3 alphaNumChar

-- functions
stepCount :: Environment -> Int
stepCount environment = length . (takeWhile ("ZZZ" /=)) $ steps environment "AAA"

steps :: Environment -> String -> [String]
steps env start = runReader (walk start 0) env

walk :: MonadReader Environment m => String -> Int -> m [String]
walk location index = do
    i <- reader (\env -> mod index (_period env))
    d <- reader (\env -> (_instructions env) !! i)
    location' <- reader (\env -> (_nodes env) M.! (location, d))
    let index' = index + 1
    liftA2 (:) (pure location) (walk location' index')

ghostSteps :: Environment -> [[String]]
ghostSteps env = map (\s -> runReader (walk s 0) env) starts
  where starts = map fst . filter (\x -> (last . fst $ x) == 'A' && L == snd x) . M.keys . _nodes $ env


ghostCount :: Environment -> Int
ghostCount env = foldl1 lcm lengths
  where counter = length . (takeWhile (\x -> last x /= 'Z'))
        lengths = map counter $ ghostSteps env


softVerify :: Environment -> [[Int]]
softVerify env = map (take 100 . apocalypse) . ghostSteps $ env


-- aka endTimes
apocalypse :: [String] -> [Int]
apocalypse ls = diffs $ 0 : (map snd $ ends)
  where ends = filter (\x -> (last . fst $ x) == 'Z') $ zip ls [0..]


diffs :: [Int] -> [Int]
diffs (x:y:xs) = (y-x) : diffs (y:xs)

-- mains

mainA :: IO ()
mainA = do
    (Just environment) <- parseInput inputP "08/input.txt"
    let answer = stepCount environment
    print answer
    -- result <- submitAnswer 2023 08 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just environment) <- parseInput inputP "08/input.txt"
    mapM_ print (softVerify environment)
    let answer = ghostCount environment
    print answer
    result <- submitAnswer 2023 08 2 answer
    print result
    return ()
