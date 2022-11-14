module Grid ( Array2
            , Ix2(..)
            , printableMap
            , printMap
            , Direction(..)
            , dirVec
            , reverseDir) where


import Utilities (CyclicEnum(..))
import Data.Massiv.Array (Array, Ix2(..))
import qualified Data.Massiv.Array as A
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Array2 a = A.Array A.U A.Ix2 a


-- directions
data Direction = North | East | South | West deriving (Bounded, Enum, Eq, Ord, Show)
instance CyclicEnum Direction


dirVec :: Direction -> (Int, Int)
dirVec d = case d of
    North -> (0, 1)
    East  -> (1, 0)
    South -> (0, -1)
    West  -> (-1, 0)


reverseDir :: Direction -> Direction
reverseDir = csucc . csucc

-- printing
-- this is the (Int, Int) version as opposed to Ix2
printableMap :: (a -> Char) -> a -> Map (Int, Int) a -> [String]
printableMap printer generic m = lls where
    xs = map fst $ M.keys m
    ys = map snd $ M.keys m
    (lx, ux) = (minimum xs, maximum xs)
    (ly, uy) = (minimum ys, maximum ys)
    ls y = [M.findWithDefault generic (x, y) m | x <- [lx..ux]]
    lls = [map printer (ls y) | y <- [ly..uy]]


printMap :: [String] -> IO ()
printMap = mapM_ putStrLn


-- sensible imports