module Puzzle_2019_12 where

import Automation (submitAnswer)
import Parsing
import Control.Monad.State
import Data.Map.Strict (Map)
import Linear.V3 (V3(..), R3, _x, _y, _z)
import Lens.Micro.Platform (Lens', (^.))
import qualified Data.Map.Strict as M

-- data types
type Point3 = V3 Int
data Moon = Moon {pos :: Point3, vel :: Point3} deriving (Eq, Ord, Show)


-- parsing
moonsP :: Parser [Moon]
moonsP = sepBy moonP newline

moonP :: Parser Moon
moonP = do
    string "<x="
    x <- integer
    string ", y="
    y <- integer
    string ", z="
    z <- integer
    string ">"
    return $ Moon (V3 x y z) (V3 0 0 0)


-- functions

step :: MonadState [Moon] m => m ()
step = gravity >> velocities


gravity :: MonadState [Moon] m => m ()
gravity = do
    ms <- get
    put $ map (attractor ms) ms


attractor :: [Moon] -> Moon -> Moon
attractor ms m = foldl attract m ms


attract :: Moon -> Moon -> Moon
attract m1 m2 = m1 {vel = (vel m1) + dv} where
    dv = fmap signum $ (pos m2) - (pos m1)


velocities :: MonadState [Moon] m => m ()
velocities = modify $ map move 


move :: Moon -> Moon
move (Moon p v) = Moon (p+v) v


energy :: Moon -> Int
energy (Moon p v) = (e p) * (e v) where
    e :: Point3 -> Int
    e p = sum (fmap abs p)


-- this is how I would write this; not the same as the reference one
-- from jhidding's 2021 Haskell aoc solutions
-- that one used Applicative and liftA2 to build the [a] from [m a]
repeatM :: Monad m => Int -> m a -> m [a]
repeatM n action
    | n <= 0    = return []
    | otherwise = do a  <- action
                     as <- repeatM (n-1) action
                     return $ a : as


-- extract :: MonadState [Moon] m => Lens' (V3 Int) Int -> m [(Int, Int)]
extract l = gets $ \ms -> map (\m -> (pos m ^. l, vel m ^. l)) ms


-- extractStep :: MonadState [Moon] m => Lens' (V3 Int) Int -> m [(Int, Int)]
extractStep l = do
    res <- extract l
    step
    return res


-- run the monadic action until the output recurs
--  s is the first time the output happened, s + t the second
repeatTimeM :: (Monad m, Ord a) => m a -> m (Int, Int)
repeatTimeM action = go 0 (M.empty) where
--     go :: Int -> Map a Int -> m (Int, Int)
    go t results = do
        res <- action
        if M.member res results
            then return (results M.! res, t)
            else go (t+1) (M.insert res t results)


timeToRepeat :: [Moon] -> Int
timeToRepeat moons = foldl1 lcm [tx, ty, tz] where
    (sx, tx) = evalState (repeatTimeM (extractStep _x)) moons
    (sy, ty) = evalState (repeatTimeM (extractStep _y)) moons
    (sz, tz) = evalState (repeatTimeM (extractStep _z)) moons



-- mains

mainA :: IO ()
mainA = do
    (Just moons) <- parseInput moonsP "12/input.txt"
    let moons' = execState (repeatM 1000 step) moons
    let answer = sum $ map energy moons'
    print answer
    -- result <- submitAnswer 2019 12 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just moons) <- parseInput moonsP "12/input.txt"
    let answer = timeToRepeat moons
    print answer
    result <- submitAnswer 2019 12 2 answer
    print result
    return ()
