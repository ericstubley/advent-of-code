module Puzzle_2019_11 where

import Automation (submitAnswer)
import Intcode
import Parsing
import Utilities (CyclicEnum(..))
import Control.Monad.State
import Data.Conduino
import Data.Map.Strict (Map)
import Data.Void (Void)
import qualified Data.Conduino.Combinators as C
import qualified Data.Conduino.Lift as L
import qualified Data.Map.Strict as M


-- data types
data Direction = North | East | South | West deriving (Bounded, Enum, Eq, Ord, Show)

instance CyclicEnum Direction

data Spaceship = Spaceship
    { hull :: Map (Int, Int) Int
    , direction :: Direction
    , plate :: (Int, Int)} deriving (Eq, Show)

-- functions

camera :: (MonadState Spaceship m) => Pipe i Int u m a
camera = C.repeatM $ do
    ship <- get
    return $ M.findWithDefault 0 (plate ship) (hull ship)


updater :: MonadState Spaceship m => Pipe Int Void u m ()
updater = do
    c <- await
    t <- await
    ship <- get
    case (c, t) of
        (Just colour, Just turnSignal) -> do
            let h' = M.insert (plate ship) colour (hull ship)
            let d' = turn turnSignal (direction ship)
            let p' = move d' (plate ship)
            modify $ \s -> Spaceship h' d' p'
            updater
        (_, _) -> return ()


-- paintAndMove :: (MonadState Spaceship m) => Int -> Pipe Int Void u m u
-- paintAndMove c = do
--     ship <- get
--     t <- await
--     case t of 
--         Nothing -> return
--         (Just t') -> do
--             let h' = M.insert (plate ship) c (hull ship)
--             let d' = turn t' (direction ship)
--             let p' = move d' (plate ship)
--             modify $ \s -> Spaceship h' d' p'
-- 
-- 
-- updater :: MonadState Spaceship m => Pipe Int Void u m u
-- updater = awaitForever paintAndMove


turn :: Int -> Direction -> Direction
turn t dir = case t of
    0 -> cpred dir
    1 -> csucc dir


move :: Direction -> (Int, Int) -> (Int, Int)
move dir (x, y) = case dir of
    North -> (x, y+1)
    East  -> (x+1, y)
    South -> (x, y-1)
    West  -> (x-1, y)


runRobot :: Program -> Int -> Spaceship
runRobot program colour = runPipePure pipeline
  where pipeline = L.execStateP initialShip $ camera
                                           .| intcodePipe program
                                           .| updater
        initialShip = Spaceship (M.singleton (0, 0) colour) North (0, 0)


-- healthCheck :: (MonadIO m, MonadState Spaceship m) => Pipe Void Void u m u
-- healthCheck = C.iterM $ do
--     s <- get
--     liftIO $ print (M.findWithDefault 0 (plate s) (hull s))
--     return ()


-- get the bounds on the hull
printableHull :: Map (Int, Int) Int -> [String]
printableHull h = lls where
    xs = map fst $ M.keys h
    ys = map snd $ M.keys h
    ls y = map charAt [(x, y) | x <- [(minimum xs)..(maximum xs)]]
    lls = [ls y | y <- reverse [(minimum ys)..(maximum ys)]]
    charAt (x, y) = if (M.findWithDefault 0 (x, y) h) == 0 
                        then ' ' 
                        else '#'


printHull :: Map (Int, Int) Int -> IO ()
printHull h = do
    let lls = printableHull h
    mapM_ print lls


-- mains

mainA :: IO ()
mainA = do
    (Just program) <- parseInput programP "11/input.txt"
    let painted = runRobot program 0
    let answer = M.size (hull painted)
    print answer
    -- result <- submitAnswer 2019 11 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just program) <- parseInput programP "11/input.txt"
    let painted = runRobot program 1
    printHull (hull painted)
    -- let answer = 0
    -- print answer
    -- result <- submitAnswer 2019 11 2 answer
    -- print result
    return ()
