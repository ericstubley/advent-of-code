module Puzzle_2018_17 where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import System.IO
import Automation (submitAnswer)

-- data types
type Parser = Parsec Void String
type GroundMap = Map (Int, Int) Tile
data Tile = Empty | Clay | StillWater | FallingWater deriving (Eq)

instance Show Tile where
    show Empty = "."
    show Clay = "#"
    show StillWater = "~"
    show FallingWater = "|"


-- parsing
parseInput :: String -> IO GroundMap
parseInput filename = do
    raw <- readFile filename
    let (Right gm) = runParser groundMapP "" raw
    return gm


groundMapP :: Parser GroundMap
groundMapP = do
    veins <- veinsP
    return $ M.unions veins


veinsP :: Parser [GroundMap]
veinsP = veinP `sepBy` char '\n'

veinP :: Parser GroundMap
veinP = do
    c1 <- char 'x' <|> char 'y'
    char '='
    n1 <- L.decimal
    string ", "
    char 'x' <|> char 'y'
    char '='
    ns <- L.decimal
    string ".."
    nf <- L.decimal
    let coords = if c1 == 'x' 
                    then [(y, n1) | y <- [ns..nf]]
                    else [(n1, x) | x <- [ns..nf]]
    return $ M.fromList (zip coords (repeat Clay))


-- functions

spring :: (Int, Int)
spring = (0, 500)

stops :: Tile -> Bool
stops t = t == Clay || t == StillWater

goes :: Tile -> Bool
goes t = t == Empty || t == FallingWater

isWater :: Tile -> Bool
isWater t = t == StillWater || t == FallingWater


fall :: GroundMap -> (Int, Int) -> GroundMap
fall gm (y, x)
    | tile == StillWater    = gm
    | y > bottom            = gm
    | alreadySeen           = gm
    | goes  below           = fall (M.insert (y, x) FallingWater gm) (y+1, x)
    | stops below           = spread gm (y, x)
    where
        tile = M.findWithDefault Empty (y, x) gm
        below = M.findWithDefault Empty (y+1, x) gm
        left = M.findWithDefault Empty (y, x-1) gm
        right = M.findWithDefault Empty (y, x+1) gm
        bottom = fst $ fst $ M.findMax gm
        alreadySeen = stops below && [tile, left, right] == (take 3 (repeat FallingWater))


spread :: GroundMap -> (Int, Int) -> GroundMap
spread gm (y, x)
    | lt == Clay  && rt == Clay  = fall (fillRow gm y lx rx StillWater) (y-1, x)
    | lt == Empty && rt == Clay  = fall (fillRow gm y lx rx FallingWater) (y, lx)
    | lt == Clay  && rt == Empty = fall (fillRow gm y lx rx FallingWater) (y, rx)
    | lt == Empty && rt == Empty = fall (fall (fillRow gm y lx rx FallingWater) (y, lx)) (y, rx)
    where
        ((ly, lx), lt) = findTerminus (-1) gm (y, x)
        ((ry, rx), rt) = findTerminus 1    gm (y, x)


fillRow :: GroundMap -> Int -> Int -> Int -> Tile -> GroundMap
fillRow gm y lx rx t = M.union (M.fromList $ zip row (repeat t)) gm
    where row = [(y, x) | x <- [lx..rx]]


findTerminus :: Int -> GroundMap -> (Int, Int) -> ((Int, Int), Tile)
findTerminus d gm (y, x)
    | stops side && stops below = ((y, x), Clay)
    | stops below && goes side  = findTerminus d gm (y, x+d)
    | goes below                = ((y, x), Empty)
    where
        side  = M.findWithDefault Empty (y, x+d) gm
        below = M.findWithDefault Empty (y+1, x) gm


countWater :: GroundMap -> Int
countWater gm = M.size $ M.filterWithKey (\k a -> isWater a && ((fst k) >= limit)) gm where
    limit = fst . fst . M.findMin $ M.filter (\a -> a == Clay) gm


countStillWater :: GroundMap -> Int
countStillWater gm = M.size $ M.filterWithKey (\k a -> a == StillWater && ((fst k) >= limit)) gm where
    limit = fst . fst . M.findMin $ M.filter (\a -> a == Clay) gm


printableGrid :: GroundMap -> [String]
printableGrid gm = map gridLine [0..bottom] where
    bottom = fst $ fst $ M.findMax gm
    lEdge = minimum $ map (\x -> snd (fst x)) (M.toList gm)
    rEdge = maximum $ map (\x -> snd (fst x)) (M.toList gm)
    gridLine :: Int -> String
    gridLine y = concat $ map (\x -> show $ M.findWithDefault Empty (y, x) gm) [lEdge..rEdge]


printGrid :: GroundMap -> IO ()
printGrid gm = mapM_ putStrLn (printableGrid gm)



-- mains

mainA :: IO ()
mainA = do
    gm <- parseInput "input.txt"
    let filled = fall gm spring
    let answer = countWater filled
    print answer
    -- result <- submitAnswer 2018 17 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    gm <- parseInput "input.txt"
    let filled = fall gm spring
    let answer = countStillWater filled
    print answer
    result <- submitAnswer 2018 17 2 answer
    print result
    return ()
