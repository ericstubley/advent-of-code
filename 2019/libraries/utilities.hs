module Utilities where


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
