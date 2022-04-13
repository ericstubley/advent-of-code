module Puzzle_2018_13 where

import Data.List (partition)
import Data.Sort (sortOn)
import Data.Massiv.Array (Array, Ix2(..))
import qualified Data.Massiv.Array as A
import System.IO
import Automation (submitAnswer)


-- data types
data Dir = U | R | D | L deriving (Eq, Ord, Enum, Bounded, Show)
data Turn = Counter | Straight | Clock deriving (Eq, Ord, Enum, Bounded, Show)
data Cart = Cart {pos :: Ix2, dir :: Dir, turn :: Turn} deriving (Eq, Show)
type Array2 a = Array A.B A.Ix2 a

-- cyclic enum
class (Eq a, Enum a, Bounded a) => CyclicEnum a where
    cpred :: a -> a
    cpred x
        | x == minBound = maxBound
        | otherwise     = pred x

    csucc :: a -> a
    csucc x
        | x == maxBound = minBound
        | otherwise     = succ x

instance CyclicEnum Dir
instance CyclicEnum Turn

-- array types



-- parsing
parseInput :: String -> IO (Array2 Char, [Cart])
parseInput filename = do
    raw <- readFile filename
    let ls = lines raw
    let originalTrack = A.fromLists' A.Seq ls
    let carts = extractCarts originalTrack
    let track = A.map cartToTrack originalTrack
    return (A.compute track, carts)


extractCarts :: Array2 Char -> [Cart]
extractCarts track = A.ifoldlS extracter [] track where
    extracter :: [Cart] -> Ix2 -> Char -> [Cart]
    extracter acc ix c
        | c == '^'  = (Cart ix U Counter) : acc
        | c == '>'  = (Cart ix R Counter) : acc
        | c == 'v'  = (Cart ix D Counter) : acc
        | c == '<'  = (Cart ix L Counter) : acc
        | otherwise = acc


cartToTrack :: Char -> Char
cartToTrack c
    | c == '>' || c == '<'  = '-'
    | c == '^' || c == 'v'  = '|'
    | otherwise             = c

-- functions
-- advances all carts
tick :: Array2 Char -> [Cart] -> ([Cart], [Cart])
tick track carts = ticker [] [] carts where
    ticker :: [Cart] -> [Cart] -> [Cart] -> ([Cart], [Cart])
    ticker collided ticked [] = (collided, ticked)
    ticker collided ticked (c:cs)
        | length (cc ++ tc ++ uc) == 0  = ticker collided (c' : ticked) cs
        | otherwise                     = ticker (collided ++ tc ++ uc ++ [c']) tu uu
        where 
            c' = advance track c
            (cc, cu) = collisionPartition c' collided
            (tc, tu) = collisionPartition c' ticked
            (uc, uu) = collisionPartition c' cs




collisionPartition :: Cart -> [Cart] -> ([Cart], [Cart])
collisionPartition c carts = partition (\x -> pos x == pos c) carts



advance :: Array2 Char -> Cart -> Cart
advance track c = Cart ix d t where
    ix = move (dir c) (pos c)
    d = orient (track A.! ix) c 
    t   | (track A.! ix) == '+' = csucc (turn c)
        | otherwise             = turn c


move :: Dir -> Ix2 -> Ix2
move d (Ix2 i j) = case d of
    U -> Ix2 (i-1) j
    R -> Ix2 i (j+1)
    D -> Ix2 (i+1) j
    L -> Ix2 i (j-1)


orient :: Char -> Cart -> Dir
orient g c = let (t, d) = (turn c, dir c) in 
    case (g, t, d) of
        ('+', Counter, _)   -> cpred d
        ('+', Straight, _)  -> d
        ('+', Clock, _)     -> csucc d
        ('/', _, U)         -> R
        ('/', _, R)         -> U
        ('/', _, D)         -> L
        ('/', _, L)         -> D
        ('\\', _, U)        -> L
        ('\\', _, R)        -> D
        ('\\', _, D)        -> R
        ('\\', _, L)        -> U
        (_, _, _)           -> d


intersect :: Turn -> Dir -> Dir
intersect t d = case t of
    Counter     -> cpred d
    Straight    -> d
    Clock       -> csucc d


firstCollision :: Array2 Char -> [Cart] -> Ix2
firstCollision track carts = let (collided, ticked) = tick track carts in
    case collided of
        []      -> firstCollision track $ sortOn (\x -> pos x) ticked
        (c:cs)  -> pos c


lastCartStanding :: Array2 Char -> [Cart] -> Ix2
lastCartStanding track carts
    | (length uncollided) == 1  = pos $ head uncollided
    | otherwise                 = lastCartStanding track $ sortOn pos uncollided
    where
        (_, uncollided) = tick track carts



mainA :: IO ()
mainA = do
    (track, carts) <- parseInput "input.txt"
    let (Ix2 i j) = firstCollision track carts
    let answer = (show j) ++ "," ++ (show i)
    print answer
    -- result <- submitAnswer 2018 13 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (track, carts) <- parseInput "input.txt"
    let (Ix2 i j) = lastCartStanding track carts
    let answer = (show j) ++ "," ++ (show i)
    print answer
    result <- submitAnswer 2018 13 2 answer
    putStrLn result
    return ()
