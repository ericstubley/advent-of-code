module Puzzle_2023_17 where

import Automation (submitAnswer)
import Parsing
import Data.Massiv.Array (Array, Ix2(..), Sz(..), (<!), (!>))
import qualified Data.Massiv.Array as A
import Control.Monad.Reader
import Data.Char (digitToInt)
import Grid (Direction(..))
import Utilities (CyclicEnum(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Applicative (liftA2)
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PQ


-- data types
type Array2 a = A.Array A.U A.Ix2 a
type Env = Array2 Int

data Marker = Marker { _ix :: Ix2
                     , _dir :: Direction
                     , _len :: Int} deriving (Eq, Ord, Show)

-- parsing
envP :: Parser Env
envP = A.fromLists' A.Seq <$> sepBy (some squareP) newline

squareP :: Parser Int
squareP = digitToInt <$> digitChar

-- functions

-- needs to store internally
-- pqueue (Int, a) to cheat past tuple ordering
-- f Map a Int
-- g Map a Int
-- path Map a a
aStar :: (Monad m, Ord a)
      => [a] -- starts
      -> (a -> m Int) -- estimation
      -> (a -> m Bool) -- termination
      -> (a -> m [a]) -- neighbours
      -> (a -> a -> m Int) -- distance
      -> m [a] -- list of states travelled through
aStar starts estimation termination neighbours distance = go pqueue f g M.empty
  where pqueue = PQ.fromList $ zip3 starts (repeat 0) starts
        f = M.fromList $ zip starts (repeat 0)
        g = M.fromList $ zip starts (repeat 0)
        paths = M.empty
        go pq f g traces = do
            let (Just (x, _, _, pq')) = PQ.minView pq
            finished <- termination x
            if finished then return (collapsePath starts traces x) else do
                xs <- neighbours x
                let currG = g M.! x
                gs <- (map (+currG)) <$> mapM (distance x) xs
                hs <- mapM estimation xs
                let xs' = zip3 xs gs hs
                let ys = filter (\(y,i,j) -> i < M.findWithDefault (maxBound :: Int) y g) xs'
                let newG = M.fromList $ map (\(y,i,_) -> (y,i)) ys
                let newF = M.fromList $ map (\(y,i,j) -> (y,i+j)) ys
                let g' = M.union newG g
                let f' = M.union newF f
                let ns = map (\(y,_,_) -> y) ys
                let pq'' = foldl inserter pq' ys
                let traces' = M.union (M.fromList $ zip ns (repeat x)) traces
                go pq'' f' g' traces'
        inserter q (y,i,j) = PQ.insert y (i+j) y q


collapsePath :: Ord a => [a] -> Map a a -> a -> [a]
collapsePath starts traces x = reverse $ go x
  where go y
            | elem y starts = [y]
            | otherwise     = y : go (traces M.! y)


heat :: MonadReader Env m => Ix2 -> m Int
heat ix = reader (\env -> env A.! ix)


est :: MonadReader Env m => Ix2 -> m Int
est (i:.j) = do
    s <- reader A.size
    let (Sz (h:.w)) = s
    return $ (h-i-1) + (w-j-1)


estB :: MonadReader Env m => Marker -> m Int
estB (Marker (i:.j) dir l) = do
    s <- reader A.size
    let (Sz (h:.w)) = s
    let e = (h-i-1) + (w-j-1)
    return $ e + (min (3-j) 0)


end :: MonadReader Env m => Ix2 -> m Bool
end (i:.j) = do
    s <- reader A.size
    let (Sz (h:.w)) = s
    return $ (h-1) == i && (w-1) == j


endB :: MonadReader Env m => Marker -> m Bool
endB (Marker ix dir j) = do
    loc <- end ix
    let len = j >= 3 && j <= 9
    return $ loc && len


inBounds :: MonadReader Env m => Ix2 -> m Bool
inBounds ix = do
    s <- reader A.size
    return $ A.isSafeIndex s ix


nextA :: MonadReader Env m => Marker -> m [Marker]
nextA (Marker ix dir j) = filterM (inBounds . _ix) candidates
  where f = Marker (move dir ix) dir (j+1)
        l = Marker (move (left dir) ix) (left dir) 0
        r = Marker (move (right dir) ix) (right dir) 0
        candidates
            | j == 2    = [l, r]
            | otherwise = [f, l, r]

left :: Direction -> Direction
left = cpred

right :: Direction -> Direction
right = csucc

move :: Direction -> Ix2 -> Ix2
move = moveWithDist 1

moveWithDist :: Int -> Direction -> Ix2 -> Ix2
moveWithDist d North (i:.j) = (i-d) :. j
moveWithDist d East  (i:.j) = i :. (j+d)
moveWithDist d South (i:.j) = (i+d) :. j
moveWithDist d West  (i:.j) = i :. (j-d)

-- nextB :: MonadReader Env m => Marker -> m [Marker]
-- nextB (Marker ix dir j) = filterM (inBounds . _ix) candidates
--   where f = Marker (move dir ix) dir (j+1)
--         l = Marker (move (left dir) ix) (left dir) 0
--         r = Marker (move (right dir) ix) (right dir) 0
--         candidates
--             | j < 3 = [f]
--             | j >= 3 && j < 9 = [f, l, r]
--             | j == 9 = [l, r]

nextB :: MonadReader Env m => Marker -> m [Marker]
nextB (Marker ix dir j) = filterM (inBounds . _ix) candidates
  where ls = map (\d -> Marker (moveWithDist d (left dir) ix) (left dir) (d-1)) [4..10]
        rs = map (\d -> Marker (moveWithDist d (right dir) ix) (right dir) (d-1)) [4..10]
        candidates = ls ++ rs

-- only use with straight line
heatDist :: MonadReader Env m => (Marker, Marker) -> m Int
heatDist (Marker (i1:.j1) _ _, Marker (i2:.j2) _ _) = sum <$> mapM heat path
  where path
            | i1 == i2 && j1 < j2 = map (\j -> (i1:.j)) [(j1+1)..j2]
            | i1 == i2 && j1 > j2 = map (\j -> (i1:.j)) [j2..(j1-1)]
            | j1 == j2 && i1 < i2 = map (\i -> (i:.j1)) [(i1+1)..i2]
            | j1 == j2 && i1 > i2 = map (\i -> (i:.j1)) [i2..(i1-1)]

heatLossA :: Env -> Int
heatLossA env = sum . tail $ hotfoot
  where starts = [Marker (0:.0) South 0, Marker (0:.0) East 0]
        prepped = aStar starts (\x -> est . _ix $ x) (\x -> end . _ix $ x) nextA (\x y -> heat . _ix $ y)
        path = runReader prepped env
        hotfoot = runReader (mapM (heat . _ix) $ path) env


heatLossB :: Env -> Int
heatLossB env = sum hotfoot
  where starts = [Marker (0:.0) South 0, Marker (0:.0) East 0]
        prepped = aStar starts (\x -> est . _ix $ x) (\x -> end . _ix $ x) nextB (curry heatDist)
        path = runReader prepped env
        hotfoot = runReader (mapM heatDist $ zip path (tail path)) env

-- mains

mainA :: IO ()
mainA = do
    (Just env) <- parseInput envP "17/input.txt"
    let answer = heatLossA env
    print answer
    -- result <- submitAnswer 2023 17 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just env) <- parseInput envP "17/input.txt"
    let answer = heatLossB env
    print answer
    -- result <- submitAnswer 2023 17 2 answer
    -- print result
    return ()
