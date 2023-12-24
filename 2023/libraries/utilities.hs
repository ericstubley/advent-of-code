module Utilities where


import Control.Applicative (liftA2)
import Data.List (foldl1')


-- minima and maxima with an ordering function
-- use the convention of the regular min vs minimum
--      min compares 2, minimum selects from a list
minWith :: Ord b => (a -> b) -> a -> a -> a
minWith f x y = if f(x) <= f(y) then x else y


maxWith :: Ord b => (a -> b) -> a -> a -> a
maxWith f x y = if f(x) >= f(y) then x else y


minimumWith :: Ord b => (a -> b) -> [a] -> a
minimumWith f xs = foldl1' chooser xs where
    chooser x y -- x is the acc argument
        | f(x) > f(y) = y
        | otherwise   = x -- if equal prefer earlier in the list


maximumWith :: Ord b => (a -> b) -> [a] -> a
maximumWith f xs = foldl1' chooser xs where
    chooser x y -- x is the acc argument
        | f(x) < f(y) = y
        | otherwise   = x -- if equal prefer earlier in the list


-- make your enum type wraparound; especially useful for 2d navigation puzzles
-- from stackexchange, from a book
class (Eq a, Enum a, Bounded a) => CyclicEnum a where
  cpred :: a -> a
  cpred d
    | d == minBound = maxBound
    | otherwise = pred d
  
  csucc :: a -> a
  csucc d
    | d == maxBound = minBound
    | otherwise = succ d


-- monadic iteration functions
-- keep repeating a monadic action until it terminates
whileM :: Monad m => m Bool -> m ()
whileM a = do
    result <- a
    if result then whileM a else return ()

-- repeat a monadic action n times, collecting the results in a list
repeatM :: Applicative m => Int -> m a -> m [a]
repeatM n a = loop n
    where loop n
            | n <= 0    = pure []
            | otherwise = liftA2 (:) a (loop (n - 1))


-- repeat an action that passes the output back in as the input
repeatThroughM :: Monad m => Int -> (a -> m a) -> a -> m [a]
repeatThroughM n action initial = loop n initial
  where loop n input
            | n <= 0    = pure [input]
            | otherwise = liftA2 (:) (pure input) (action input >>= loop (n-1))


partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM predicate [] = return ([], [])
partitionM predicate (x:xs) = do
    result <- predicate x
    (ayes, nays) <- partitionM predicate xs
    if result
        then return (x:ayes, nays)
        else return (ayes, x:nays)