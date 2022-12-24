module Puzzle_2022_23 where

import Automation (submitAnswer)
import Parsing
import Data.Massiv.Array (Array, Ix2(..))
import qualified Data.Massiv.Array as A
import Control.Monad.State

-- data types
data Tile = Empty | Elf | North | South | West | East 
    deriving (Eq, Ord, Enum, Bounded)

instance Show Tile where
    show Empty = "."
    show Elf   = "#"
    show North = "N"
    show South = "S"
    show West  = "W"
    show East  = "E"

type Grove = Array A.B A.Ix2 Tile

-- parsing
groveP :: Parser Grove
groveP = A.fromLists' A.Seq <$> sepBy rowP newline


rowP :: Parser [Tile]
rowP = some cellP


cellP :: Parser Tile
cellP = (char '#' >> return Elf) <|> (char '.' >> return Empty)


-- functions
simulate :: Grove -> Int -> Grove
simulate grove n = execState (steps n) grove


steps :: MonadState Grove m => Int -> m ()
steps limit = go 0 where
    go n
        | n == limit = return ()
        | otherwise  = step n >> go (n+1)


-- one step of the process
step :: MonadState Grove m => Int -> m ()
step n = expand >> propose n >> resolve >> trim


stepCheck :: MonadState Grove m => Int -> m Bool
stepCheck n = do
    grove <- get
    step n
    grove' <- get
    return $ A.eqArrays (==) grove grove'


stabilize :: Grove -> Int
stabilize grove = evalState stabilizer grove


stabilizer :: MonadState Grove m => m Int
stabilizer = go 0 where
    go n = do
        check <- stepCheck n
        if check
            then return (n+1)
            else go (n+1)


-- if necessary, expand the grove so that there's an empty edge everywhere
expand :: MonadState Grove m => m ()
expand = modify $ A.compute . A.applyStencil 
            (A.Padding (A.Sz2 1 1) (A.Sz2 1 1) (A.Fill Empty))
            A.idStencil


-- for round n, generate the proposed moves out of the current state
propose :: MonadState Grove m => Int -> m ()
propose n = modify $ A.compute . A.mapStencil (A.Fill Empty) (proposalStencil n)


proposalStencil :: Int -> A.Stencil Ix2 Tile Tile
proposalStencil n = A.makeStencil (A.Sz (3 :. 3)) (1 :. 1) go
  where go :: (Ix2 -> Tile) -> Tile
        go get
            | get (0 :. 0) == Empty                = Empty
            | all (== Empty) $ map get neighbours1 = Elf
            | otherwise                            = mover (moveOrder n) get
        mover :: [Tile] -> (Ix2 -> Tile) -> Tile
        mover [] get = Elf
        mover (m:ms) get
            | all (== Empty) $ map get (visibleNeighbours m) = m
            | otherwise                                      = mover ms get



-- take in a set of proposed moves and resolve them, updating the state
resolve :: MonadState Grove m => m ()
resolve = modify $ A.compute . A.mapStencil (A.Fill Empty) resolutionStencil


-- if marked as stay then definitely elf
-- if marked as empty, then check for exactly one among cardinal neighbours, else blank
-- if marked as north, south, east, west, check if it's moving, else ELF
resolutionStencil :: A.Stencil Ix2 Tile Tile
resolutionStencil = A.makeStencil (A.Sz (5 :. 5)) (2 :. 2) go
  where go :: (Ix2 -> Tile) -> Tile
        go get
            | get (0 :. 0) == Elf   = Elf
            | get (0 :. 0) == Empty = resolveEmpty get
            | otherwise             = resolveMove (get (0 :. 0)) get


-- given a tile which is marked Blank, decide if it gets filled or not
resolveEmpty :: (Ix2 -> Tile) -> Tile
resolveEmpty get
    | proposedMoves == 1 = Elf
    | otherwise          = Empty
      where attempts = zipWith (==) [South, North, East, West] (map get cardinalNeighbours)
            proposedMoves = length . filter id $ attempts


-- given a tile which is marked NSWE, decide if it moves or not
resolveMove :: Tile -> (Ix2 -> Tile) -> Tile
resolveMove m get
    | otherMoves == 0 = Empty
    | otherwise       = Elf
      where attempts = zipWith (==) (competingMoves m) (map get $ competingNeighbours m)
            otherMoves = length . filter id $ attempts


-- trim down border rows/columns which are empty
trim :: MonadState Grove m => m ()
trim = do
    start <- gets convexHullNW
    end   <- gets convexHullSE 
    modify $ A.compute . A.extractFromTo' start end


-- returns i :. j where row i is the first with an Elf, sim column j
convexHullNW :: Grove -> Ix2
convexHullNW grove = i :. j where
    i = head $ dropWhile (\x -> A.all (==Empty) (grove A.!> x)) [0..]
    j = head $ dropWhile (\x -> A.all (==Empty) (grove A.<! x)) [0..]


-- return i :. j where row i is the last row with an elf, sim column j
convexHullSE :: Grove -> Ix2
convexHullSE grove = (i+1) :. (j+1) where
    A.Sz (rows :. cols) = A.size grove
    i = head $ dropWhile (\x -> A.all (==Empty) (grove A.!> x)) (reverse [0.. (rows-1)])
    j = head $ dropWhile (\x -> A.all (==Empty) (grove A.<! x)) (reverse [0.. (cols-1)])


countEmpty :: Grove -> Int
countEmpty grove = A.sum $ A.map (\t -> if t == Empty then 1 else 0) grove



visibleNeighbours :: Tile -> [Ix2]
visibleNeighbours m = case m of
                        North -> [-1 :. -1, -1 :.  0, -1 :.  1]
                        South -> [ 1 :. -1,  1 :.  0,  1 :.  1]
                        West  -> [-1 :. -1,  0 :. -1,  1 :. -1]
                        East  -> [-1 :.  1,  0 :.  1,  1 :.  1]


competingNeighbours :: Tile -> [Ix2]
competingNeighbours m = case m of
                            North -> [-1 :. -1, -2 :.  0, -1 :.  1]
                            South -> [ 1 :. -1,  2 :.  0,  1 :.  1]
                            West  -> [-1 :. -1,  0 :. -2,  1 :. -1]
                            East  -> [-1 :.  1,  0 :.  2,  1 :.  1]


competingMoves :: Tile -> [Tile]
competingMoves m = case m of
                    North -> [East, South, West]
                    South -> [East, North, West]
                    West  -> [South, East, North]
                    East  -> [South, West, North]


cardinalNeighbours :: [Ix2]
cardinalNeighbours = [-1 :. 0, 1 :. 0, 0 :. -1, 0 :. 1]


neighbours1 :: [Ix2]
neighbours1 = [ -1 :. -1, -1 :. 0, -1 :. 1
              ,  0 :. -1,           0 :. 1
              ,  1 :. -1,  1 :. 0,  1 :. 1]


neighbours2 :: [Ix2]
neighbours2 = [                    -2 :. 0
              ,          -1 :. -1, -1 :. 0, -1 :. 1
              , 0 :. -2,  0 :. -1,           0 :. 1, 0 :. 2
              ,           1 :. -1,  1 :. 0,  1 :. 1
              ,                     2 :. 0                 ]




moveOrder :: Int -> [Tile]
moveOrder n = take 4 . drop n $ cycle [North, South, West, East]


printableGrove :: Grove -> [String]
printableGrove g = map (concat . map show) (A.toLists2 g)


printGrove :: Grove -> IO ()
printGrove g = mapM_ putStrLn (printableGrove g)

-- mains

mainA :: IO ()
mainA = do
    (Just grove) <- parseInput groveP "23/input.txt"
    let answer = countEmpty $ simulate grove 10
    print answer
    -- result <- submitAnswer 2022 23 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just grove) <- parseInput groveP "23/input.txt"
    let answer = stabilize grove
    print answer
    -- result <- submitAnswer 2022 23 2 answer
    -- print result
    return ()
