module Puzzle_2023_24 where

import Automation (submitAnswer)
import Parsing
import Utilities (combinations)

import Math.NumberTheory.Factor (pfactors)
import Lens.Micro.Platform
import Linear


-- data types
type Hail2 = (V2 Int, V2 Int)
type Hail3 = (V3 Int, V3 Int)

type Hail3R = (V3 Rational, V3 Rational)

-- parsing
hailstonesP :: Parser [Hail3]
hailstonesP = sepBy hailstoneP newline

-- 19, 13, 30 @ -2,  1, -2
hailstoneP :: Parser Hail3
hailstoneP = do
    px <- integer
    char ',' >> space
    py <- integer
    char ',' >> space
    pz <- integer
    space >> char '@' >> space
    vx <- integer
    char ',' >> space
    vy <- integer
    char ',' >> space
    vz <- integer
    return $ (V3 px py pz, V3 vx vy vz)


-- functions
testIntersect :: Rational -> Rational -> Hail2 -> Hail2 -> Bool
testIntersect lower upper a b
    | parallel a b = False
    | otherwise    = futureA && futureB && result
      where (t, s, d) = intersect a b
            futureA = (toRational t / toRational d) >= 0
            futureB = (toRational s / toRational d) >= 0
            (rx, ry) = position a (t, d)
            result = lower <= rx && rx <= upper && lower <= ry && ry <= upper


position :: Hail2 -> (Int, Int) -> (Rational, Rational)
position (V2 px py, V2 vx vy) (t, d) = (rx, ry)
  where mult = toRational t / toRational d
        rx = toRational px + mult * toRational vx
        ry = toRational py + mult * toRational vy


parallel :: Hail2 -> Hail2 -> Bool
parallel (_, V2 vx vy) (_, V2 wx wy) = vx*wy - vy*wx == 0

intersect :: Hail2 -> Hail2 -> (Int, Int, Int)
intersect (V2 px py, V2 vx vy) (V2 qx qy, V2 wx wy) = (t, s, det)
  where det = vy*wx - vx*wy
        t = (-1)*wy*(qx - px) + wx*(qy - py)
        s = (-1)*vy*(qx - px) + vx*(qy - py)

collisions :: Rational -> Rational -> [Hail3] -> Int
collisions lower upper hail = length . filter (uncurry $ testIntersect lower upper) $ pairs
  where hail' = map (\t -> (t ^. _1 . _xy, t ^. _2 . _xy)) hail
        pairs = combinations hail'


-- part b

-- solveB :: [Hail3] -> 



vel :: Hail3R -> Hail3R -> Hail3R -> V3 Rational
vel (p1, v1) (p2, v2) (p3, v3) = v 
  where r12 = cross (p1 ^-^ p2) (v1 ^-^ v2)
        r13 = cross (p1 ^-^ p3) (v1 ^-^ v3)
        r23 = cross (p2 ^-^ p3) (v2 ^-^ v3)
        m12 = dot (p1 ^-^ p2) (cross v1 v2)
        m13 = dot (p1 ^-^ p3) (cross v1 v3)
        m23 = dot (p2 ^-^ p3) (cross v2 v3)
        matrix = V3 r12 r13 r23
        col = V3 m12 m13 m23
        v = (inv33 matrix) !* col

pos :: Hail3R -> Hail3R -> V3 Rational -> V3 Rational
pos (p1, v1) (p2, v2) v = fmap (\x -> x / scale) p
  where w1 = v1 ^-^ v
        w2 = v2 ^-^ v
        ww = cross w1 w2
        r = dot ww (cross p2 w2)
        s = (-1) * (dot ww (cross p1 w1))
        t = dot ww p1
        scale = dot ww ww
        matrix = transpose $ V3 w1 w2 ww
        col = V3 r s t
        p = matrix !* col



solveB :: [Hail3] -> Rational
solveB hail = sum p
  where hail' = map (\t -> (fmap toRational $ fst t, fmap toRational $ snd t)) hail
        v = vel (hail' !! 0) (hail' !! 1) (hail' !! 2)
        p = pos (hail' !! 0) (hail' !! 1) v

-- mains

mainA :: IO ()
mainA = do
    (Just hail) <- parseInput hailstonesP "24/input.txt"
    let answer = collisions 200000000000000 400000000000000 hail
    print answer
    -- result <- submitAnswer 2023 24 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just hail) <- parseInput hailstonesP "24/input.txt"
    let answer = solveB hail
    print answer
    -- result <- submitAnswer 2023 24 2 answer
    -- print result
    return ()
