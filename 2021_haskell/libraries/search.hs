module Search ( search
              , binarySearch
              , linearSearchForward
              , linearSearchBackward
              , bfs
              , dfs ) where


import Data.Map.Strict (Map)
import Data.Maybe (Maybe(..))
import Data.Sequence (Seq(..), (><))
import Data.Set (Set)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Sq
import qualified Data.Set as S


-- other things
-- dijkstra/astar
-- floyd-warshall: 
-- in a graph with vertices labelled 1 through n
--  the shortest path i -> j using only vertices up to k (sp(i, j, k)) is either
--      a path that doesn't go through k, i.e. sp(i, j, k-1)
--      or a concatenation of paths through k, i.e. sp(i, k, k-1) + sp(k, j, k-1)
--  find this recursively, shortest path is sp(i, j, n)
--  sp(i, j, 0) = weight(i, j)
--  sp(i, j, k) = min(sp(i, j, k-1), sp(i, k, k-1) + sp(k, j, k-1))


-- search through a list, which can be customized to binary, linear, etc.
-- the list must be monotically False to monotically True
-- returns the pair of elements at which the switch happens
-- use at your own risk! if your update function is bad in some way
search :: (Int -> Int -> Int) -> (a -> Bool) -> [a] -> (a, a)
search update predicate xs = go 0 ((length xs) - 1) where
    go :: Int -> Int -> (a, a)
    go lo hi
        | hi == lo + 1           = (xs !! lo, xs !! hi)
        | predicate mid == False = go mid hi
        | predicate mid == True  = go lo mid where
            mid = update lo hi

binarySearch :: (a -> Bool) -> [a] -> (a, a)
binarySearch = search (\lo hi -> div (lo + hi) 2)

linearSearchForward :: (a -> Bool) -> [a] -> (a, a)
linearSearchForward = search (\lo hi -> lo + 1)

linearSearchBackward :: (a -> Bool) -> [a] -> (a, a)
linearSearchBackward = search (\lo hi -> hi - 1)


-- initial state
-- function which detects termination state
-- function which generates next states from current one

bfs :: Monad m , Ord a =>
       a ->
       (a -> m Bool) ->
       (a -> m [a]) ->
       (a -> )
       m a
bfs initial terminator generator = go S.empty (Sq.singleton initial) where
    go :: Monad m => Set a -> Seq a -> m a
    go seen (state :<| queue) = do
        if S.member state seen then go seen queue else do
            finished <- terminator state
            if finished then return state else do
                options <- generator state
                go (S.insert state seen) (queue >< $ Sq.fromList options)


-- potential problems
-- size of the set blows up too much without filtering
-- 



-- initial state
-- function which detects termination state, returns a Maybe?
--      Nothing for not a termination state
--      Just b for some function a -> b of state -> termination value
-- function which generates next states from current one
-- function which selects the best state based on the dfs values
-- function which computes the new value based off of the current state,
--      along with the values of the substates

dfs :: Monad m , Ord a =>
       a ->
       (a -> m (Maybe b)) ->
       (a -> m [a]) ->
       (a -> [b] -> m b) ->
       m b
dfs initial terminator generator computer = go initial where
    memo = M.empty
    go :: Monad m, Ord a=> a -> m b
    go state = do
        if M.member state memo then return (memo M.! state) else do
            case terminator state of
                (Just x) -> do
                    M.insert state x memo
                    return x
                Nothing  -> do
                    options <- generator state
                    values <- mapM go options
                    value <- computer state values
                    M.insert state value memo
                    return value

-- potential problems
-- too annoying to write 4 functions to pass through?


-- 
floydWarshall :: Ord a => 
                 (a -> a -> Maybe Int) -> 
                 [a] -> 
                 Map (a, a) (Maybe Int)
floydWarshall weight nodes = foldl go start tries where
    start = M.fromList $ map (\t -> (t, weight (fst t) (snd t))) (pairs nodes)
    tries = [(k, i, j) | k <- nodes, i <- nodes, j <- nodes]
    go :: Ord a => Map (a, a) (Maybe Int) -> (a, a, a) -> Map (a, a) (Maybe Int)
    go state (k, i, j) = do
        curr <- M.find (i, j) state
        leg1 <- M.find (i, k) state
        leg2 <- M.find (j, k) state
        return $ M.insert (i, j) 

-- how to correctly handle the maybes here?
-- the logic you want: replace the value at (i, j) with
-- min curr (leg1 + leg2), where Nothing > Just x for any x
