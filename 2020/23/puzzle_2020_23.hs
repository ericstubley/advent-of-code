module Puzzle_2020_23 where

import Automation (submitAnswer)
import Data.Foldable (toList)
import Data.Sequence (Seq(..), (|>), (<|), (><))
import qualified Data.Sequence as S

-- data types
type Cups = Seq Int

-- functions
layout :: Int -> Cups
layout 0 = Empty
layout n = layout (div n 10) |> (mod n 10)


after1 :: Cups -> Int
after1 (1:<|cs) = go 0 cs
  where go :: Int -> Cups -> Int
        go n Empty = n
        go n (c:<|cs) = go (10*n + c) cs
after1 cs = after1 $ rotate cs


rotate :: Seq a -> Seq a
rotate Empty = Empty
rotate (a:<|s) = s |> a


-- take 3
-- compute next
-- find next
-- insert 3
-- rotate
shuffle :: Cups -> Cups
shuffle cups@(curr:<|rest) = rest'' |> curr
  where (temp, rest') = S.splitAt 3 rest
        dest = destination curr temp
        (Just ix) = S.elemIndexL dest rest'
        (before, after) = S.splitAt (ix+1) rest'
        rest'' = before >< temp >< after


destination :: Int -> Cups -> Int
destination curr temp
    | search == Nothing = next
    | otherwise         = destination next temp
      where next = center (curr-1)
            search = S.elemIndexL next temp


center :: Int -> Int
center n
    | n < 1     = n + 9
    | n > 9     = n - 9
    | otherwise = n

-- mains

mainA :: IO ()
mainA = do
    let answer = after1 $ iterate shuffle (layout 137826495) !! 100
    print answer
    -- result <- submitAnswer 2020 23 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    let answer = 0
    print answer
    -- result <- submitAnswer 2020 23 2 answer
    -- print result
    return ()
