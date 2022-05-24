module Puzzle_2019_15 where

import Automation (submitAnswer)
import Grid
import Intcode
import Parsing
import Control.Monad.State
import Data.Conduino
import Data.Maybe
import Data.Map.Strict (Map)
import Data.Sequence (Seq(..), (><))
import Data.Set (Set)
import Data.Void (Void)
import qualified Data.Conduino.Combinators as C
import qualified Data.Conduino.Lift as L
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S

-- data types

data Tile = Wall | Open | Tank deriving (Enum, Eq, Ord, Show)

tilePrinter :: Tile -> Char
tilePrinter t = case t of
                Wall -> '#'
                Open -> '.'
                Tank -> '@'

data Environment = Environment
    { maze :: Map (Int, Int) Tile
    , pos :: (Int, Int)
    , path :: [Direction]
    , command :: Maybe Direction} deriving (Eq, Show)


-- functions

-- | priorities
-- explore new directions
-- backtrack if list is non empty
-- exit, i.e. issue 
commander :: (MonadState Environment m) => Pipe () (Maybe Direction) Void m ()
commander = C.repeatM $ do
    env <- get
    let (x, y) = pos env
    command' <- selectDir North
    case (command', path env) of
        (Just d, _) -> do
            put $ env {command = command'}
            return command'
        (Nothing, []) -> do
            put $ env {command = command'}
            return command'
        (Nothing, _) -> do 
            let path' = tail (path env)
            let command'' = Just (head (path env))
            put $ env {path = path', command = command''}
            return command''


selectDir :: (MonadState Environment m) => Direction -> m (Maybe Direction)
selectDir d = do
    env <- get
    let (x, y) = pos env
    let (dx, dy) = dirVec d
    if not $ M.member (x+dx, y+dy) (maze env)
        then return (Just d)
        else if d == (maxBound :: Direction)
            then return Nothing
            else selectDir (succ d)


translateCommand :: Maybe Direction -> Int
translateCommand d = case d of
                        (Just North) -> 1
                        (Just East)  -> 4
                        (Just South) -> 2
                        (Just West)  -> 3
                        Nothing      -> 0


translateResponse :: Int -> Tile
translateResponse i = case i of
                        0 -> Wall
                        1 -> Open
                        2 -> Tank



updateHandler :: (MonadState Environment m) => Pipe Int Void () m ()
updateHandler = awaitForever updater


-- | priorities
-- if the command was nothing, don't make any changes
-- if the command was a movement
--      if we hit a wall, just add in maze info
--      if we moved, and got new info: update maze, path, pos
--      if we moved to a place we know; just update pos
--      update pos
--      if we learned something about a new position update maze
updater :: (MonadState Environment m) => Int -> Pipe Int Void () m ()
updater i = do
    env <- get

    case command env of
        Nothing -> do
            return ()
        Just d  -> do
            let r = translateResponse i
            let (x, y) = pos env
            let (dx, dy) = dirVec d
            let pos' = (x+dx, y+dy)
            let path' = (reverseDir d) : path env
            let maze' = M.insert pos' r (maze env)
            case (M.member pos' (maze env), r) of
                (True, _)     -> put $ env {pos=pos'}
                (False, Wall) -> put $ env {maze=maze'}
                (False, _)    -> put $ env {maze=maze', pos=pos', path=path'}


runDroid :: Program -> Environment
runDroid program = runPipePure $ L.execStateP env pipeline where
    pipeline = mapOutput translateCommand commander
              .| void (intcodePipe program)
              .| updateHandler
    env = Environment (M.singleton (0, 0) Open) (0, 0) [] Nothing


-- shortest path from (0, 0) to Tank location
shortestPath :: Map (Int, Int) Tile -> (Int, Int) -> (Int, Int) -> Int
shortestPath maze start end = go (Seq.fromList [(start, 0)]) (S.singleton start) where
    go :: Seq ((Int, Int), Int) -> Set (Int, Int) -> Int
    go (((x, y), d) :<| queue) seen
        | (x, y) == end = d
        | otherwise     = go (queue >< q') (S.insert (x, y) seen)
          where
            nbXY = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
            pathXY = filter (\p -> (M.findWithDefault Wall p maze) /= Wall) nbXY
            newXY = filter (\p -> not $ S.member p seen) pathXY
            q' = Seq.fromList $ zip newXY (repeat (d+1))


-- super quick longestPath hack of the shortestPath algorithm
longestPath :: Map (Int, Int) Tile -> (Int, Int) -> Int
longestPath maze start = go (Seq.fromList [(start, 0)]) (S.singleton start) 0 where
    go :: Seq ((Int, Int), Int) -> Set (Int, Int) -> Int -> Int
    go Empty _ d' = d'
    go (((x, y), d) :<| queue) seen d' = go (queue >< q') (S.insert (x, y) seen) (max d d')
      where
        nbXY = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
        pathXY = filter (\p -> (M.findWithDefault Wall p maze) /= Wall) nbXY
        newXY = filter (\p -> not $ S.member p seen) pathXY
        q' = Seq.fromList $ zip newXY (repeat (d+1))


findTank ::  Map (Int, Int) Tile -> (Int, Int)
findTank m = head . M.keys $ M.filter (==Tank) m

-- mains

mainA :: IO ()
mainA = do
    (Just repairBot) <- parseInput programP "15/input.txt"
    let env = runDroid repairBot
    printMap $ printableMap tilePrinter Wall (maze env)
    let tank = findTank (maze env)
    let answer = shortestPath (maze env) (0, 0) tank
    print answer
    -- result <- submitAnswer 2019 15 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just repairBot) <- parseInput programP "15/input.txt"
    let env = runDroid repairBot
    let tank = findTank (maze env)
    let answer = longestPath (maze env) tank
    print answer
    -- result <- submitAnswer 2019 15 2 answer
    -- print result
    return ()
