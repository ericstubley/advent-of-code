module Puzzle_2018_10 where

import Data.Massiv.Array (Array, Ix2(..))
import qualified Data.Massiv.Array as A
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import System.IO
-- import Automation (submitAnswer)

-- data types
type Array2 a = Array A.U A.Ix2 a

type Parser = Parsec Void String

data Point = Point {x::Int, y::Int} deriving (Show, Eq)


-- parsing
parseInput :: String -> IO ([Point], [Point])
parseInput filename = do
    raw <- readFile filename
    let (Right parsed) = runParser linesParser "" raw
    return (map fst parsed, map snd parsed)


linesParser :: Parser [(Point, Point)]
linesParser = lineParser `sepBy` char '\n'


-- position=< 30610,  30572> velocity=<-3, -3>
lineParser :: Parser (Point, Point)
lineParser = do
    string "position=<"
    px <- spaceToSigned
    char ','
    py <- spaceToSigned
    string "> velocity=<"
    vx <- spaceToSigned
    char ','
    vy <- spaceToSigned
    char '>'
    return (Point px py, Point vx vy)

spaceToSigned :: Parser Int
spaceToSigned = skipManyTill (space) (L.signed space L.decimal)


-- functions
printGrid :: [Point] -> IO ()
printGrid ps = do
    let (Point mx my) = tl ps
    let basedPs = map (reBase (mx, my)) ps
    let printer = printHelper basedPs
    let (w, h) = dimensions ps
    let grid = A.makeArray A.Seq (A.Sz (h :. w)) printer :: Array2 Char
    mapM_ print (A.toLists grid)
    return ()


printHelper :: [(Int, Int)] -> Ix2 -> Char
printHelper ps ix
    | A.fromIx2 ix `elem` ps    = '#'
    | otherwise                 = '.'


reBase :: (Int, Int) -> Point -> (Int, Int)
reBase (bx, by) p = ((y p) - by, (x p) - bx)


quickAdvance :: Int -> [Point] -> [Point] -> (Int, [Point])
quickAdvance limit vs ps
    | maximum (dimensions ps) <= limit  = (0, ps)
    | otherwise                         = (1 + t, ps')
    where
        (t, ps') = quickAdvance limit vs $ advance vs ps


dimensions :: [Point] -> (Int, Int)
dimensions ps = (width, height) where
    width   = (x $ br ps) - (x $ tl ps) + 1
    height  = (y $ br ps) - (y $ tl ps) + 1


tl :: [Point] -> Point
tl ps = Point (minimum $ map x ps) (minimum $ map y ps)

br :: [Point] -> Point
br ps = Point (maximum $ map x ps) (maximum $ map y ps)


advance :: [Point] -> [Point] -> [Point]
advance vs ps = zipWith add vs ps


advanceN :: Int -> [Point] -> [Point] -> [Point]
advanceN n vs ps = advance nVs ps where
    nVs = map (\p -> Point (n * (x p)) (n * (y p))) vs


add :: Point -> Point -> Point
add p1 p2 = Point (x p1 + x p2) (y p1 + y p2)


-- mains

mainA :: IO ()
mainA = do
    (ps, vs) <- parseInput "input.txt"
    let baps = advanceN 10000 vs ps
    let (t1, qaps) = quickAdvance 150 vs baps
    t2 <- manualAdvancer 0 vs qaps
    print $ 10000 + t1 + t2
    -- print answer
    -- result <- submitAnswer 2018 10 1 answer
    -- print result
    return ()

manualAdvancer :: Int -> [Point] -> [Point] -> IO Int
manualAdvancer t vs ps = do
    printGrid ps
    putStrLn "Should we continue?"
    confirm <- getLine 
    if confirm == "y"
        then manualAdvancer (t+1) vs $ advance vs ps
        else return t


mainB :: IO ()
mainB = do
    -- let answer = 0
    -- print answer
    -- result <- submitAnswer 2018 10 2 answer
    -- print result
    return ()
