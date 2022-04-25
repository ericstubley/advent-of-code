module Puzzle_2018_25 where

import Data.List (partition)
import Data.Set (Set)
import Data.Vector.Unboxed (Vector)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V
import qualified Text.Megaparsec.Char.Lexer as L
import System.IO
import Automation (submitAnswer)

-- data types
type Parser = Parsec Void String
type Star = Vector Int
type Constellation = Set Star
type Starmap = Set Constellation
-- type Constellation = [Star]
-- type Starmap = [Constellation]


-- parsing
parseInput :: String -> IO [Star]
parseInput filename = do
    raw <- readFile filename
    let (Right stars) = runParser inputP "" raw
    return stars


inputP :: Parser [Star]
inputP = starP `sepBy` newline


starP :: Parser Star
starP = do
    x <- integerP
    char ','
    y <- integerP
    char ','
    z <- integerP
    char ','
    t <- integerP
    return $ V.fromList [x, y, z, t]


integerP :: Parser Int
integerP = L.signed space L.decimal


-- functions
constellations :: [Star] -> Starmap
constellations stars = go stars S.empty where
    go :: [Star] -> Starmap -> Starmap
    go [] cs = cs
    go (s:rs) cs = go rs (S.insert joined far) where
        (nearby, far) = S.partition (near s) cs
        joined = S.insert s $ S.unions nearby
-- constellations stars = go stars [] where
--     go :: [Star] -> Starmap -> Starmap
--     go [] cs = cs
--     go (s:rs) cs = go rs (joined : far) where
--         (nearby, far) = partition (near s) cs
--         joined = s : (concat nearby)



manhattan :: Star -> Star -> Int
manhattan s t = V.sum $ V.zipWith (\x y -> abs(x-y)) s t


near :: Star -> Constellation -> Bool
near s c = S.member True $ S.map (\p -> manhattan p s <= 3) c
-- near s [] = False
-- near s (p:c) = if manhattan s p <= 3
--     then True
--     else near s c


-- mains

mainA :: IO ()
mainA = do
    stars <- parseInput "input.txt"
    let cs = constellations stars
    let answer = S.size cs
    -- let answer = length cs
    print answer
    -- result <- submitAnswer 2018 25 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    let answer = 0
    print answer
    -- result <- submitAnswer 2018 25 2 answer
    -- print result
    return ()
