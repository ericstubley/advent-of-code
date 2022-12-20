module Puzzle_2022_20 where

import Automation (submitAnswer)
import Parsing
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Sort (sortOn)
import Data.Foldable (toList)
import Data.Maybe (fromJust)

import Data.Sequence (Seq(..), (|>), (><))
import qualified Data.Sequence as S


-- data types
-- map original index -> (value, currentIndex)
type File = Seq (Int, Int)

-- parsing
fileP :: Parser [Int]
fileP = sepBy integer newline

-- functions
prepare :: [Int] -> File
prepare is = S.fromList $ zip is [0..]


mix :: File -> File
mix file = go file 0 (S.length file)


go :: File -> Int -> Int -> File
go f n limit
    | n == limit = f
    | otherwise  = go f' (n+1) limit
      where 
        m = S.length f
        old = fromJust $ S.findIndexL (\x -> snd x == n) f
        (jump, _) = S.index f old
        new = mod (old + jump) (m-1)
        (before, after) = S.splitAt new $ S.deleteAt old f
        f' = (before |> (jump, n)) >< after



rotateTo :: (a -> Bool) -> Seq a -> Seq a
rotateTo p (x :<| xs)
    | p x = (x :<| xs)
    | otherwise = rotateTo p (xs :|> x)



-- shuffle :: Int -> Int -> Int -> Int
-- shuffle old new current
--     | old == current                  = new
--     | old < current && current < new  = current - 1
--     | new < current && current < old  = current + 1
--     | old < current && current == new = current - 1
--     | current < old && current == new = current + 1
--     | otherwise                       = current


reindex :: File -> File
reindex file = rotateTo (\x -> fst x == 0) file


find :: File -> Int -> Int
find file i = fst $ S.index file i


finalList :: [Int] -> [Int]
finalList is = map fst . toList . reindex . mix . prepare $ is


intermediateList :: [Int] -> Int -> [Int]
intermediateList is n = map fst . toList . (\f -> go f 0 n) . prepare $ is


groveCoordinateSum :: [Int] -> Int
groveCoordinateSum is = (find f i1) + (find f i2) + (find f i3) where
    m = length is
    f = reindex . mix . prepare $ is
    i1 = mod 1000 m
    i2 = mod 2000 m
    i3 = mod 3000 m


decryptionKey :: Int
decryptionKey = 811589153


prePrepare :: [Int] -> [Int]
prePrepare = map ((*) decryptionKey)


finalList' :: [Int] -> Int -> [Int]
finalList' is n = map fst . toList . reindex . mixN n . prepare . prePrepare $ is


mixN :: Int -> File -> File
mixN n f = head . drop n . iterate mix $ f


groveCoordinateSum' :: [Int] -> Int
groveCoordinateSum' is = (find f i1) + (find f i2) + (find f i3) where
    m = length is
    f = reindex . mixN 10 . prepare . prePrepare $ is
    i1 = mod 1000 m
    i2 = mod 2000 m
    i3 = mod 3000 m


-- mains

mainA :: IO ()
mainA = do
    (Just file) <- parseInput fileP "20/input.txt"
    let answer = groveCoordinateSum file
    print answer
    -- result <- submitAnswer 2022 20 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just file) <- parseInput fileP "20/input.txt"
    let answer = groveCoordinateSum' file
    print answer
    -- result <- submitAnswer 2022 20 2 answer
    -- print result
    return ()
