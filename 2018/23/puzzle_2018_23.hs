module Puzzle_2018_23 where

import Data.Bits
import Data.List (foldl1', maximumBy)
import Data.Set (Set)
import Data.Sort (sortOn)
import Data.Massiv.Core.Index
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Set as S
import System.IO
import System.Random.Stateful
import Automation (submitAnswer)

-- data types
type Parser = Parsec Void String
type Nanobot = (Int, Ix3)

data Octant = Octant {lower :: Ix3, upper :: Ix3} deriving (Eq, Ord, Show)


-- parsing
parseInput :: String -> IO [Nanobot]
parseInput filename = do
    raw <- readFile filename
    let (Right bots) = runParser botsP "" raw
    return bots

botsP :: Parser [Nanobot]
botsP = botP `sepBy` char '\n'

-- pos=<61296484,84302508,39845359>, r=73484485
botP :: Parser Nanobot
botP = do
    string "pos=<"
    x <- signed
    char ','
    y <- signed
    char ','
    z <- signed
    string ">, r="
    r <- L.decimal
    return (r, x:>y:.z)

signed :: Parser Int
signed = L.signed space L.decimal


-- functions
manhattan :: Ix3 -> Ix3 -> Int
manhattan (a:>b:.c) (x:>y:.z) = abs (a-x) + abs (b-y) + abs (c-z)


inRange :: Nanobot -> Ix3 -> Bool
inRange (r, p) p' = (manhattan p p') <= r



inRangeOfMax :: [Nanobot] -> Int
inRangeOfMax bots = length . (filter (\x -> inRange topBot (snd x))) $ bots where
    topBot = maximum bots


-- part b experimenting

rangeOverlap :: Nanobot -> Nanobot -> Bool
rangeOverlap (r, p) (r', p') = (manhattan p p') <= (r + r')


overlappingPairs :: [Nanobot] -> [(Nanobot, Nanobot)]
overlappingPairs bots = [(a, b) | a <- bots, b <- bots, a /= b, rangeOverlap a b]


overlappingTriples :: [Nanobot] -> [(Nanobot, Nanobot, Nanobot)]
overlappingTriples bots = [(a, b, c) |
    a <- bots,
    b <- bots,
    c <- bots,
    a /= b,
    a /= c,
    b /= c,
    rangeOverlap a b,
    rangeOverlap a c,
    rangeOverlap b c]


distToRange :: Nanobot -> Ix3 -> Int
distToRange (r, p) p' = if d <= r then 0 else d - r where
    d = manhattan p p'


botsInRange :: [Nanobot] -> Ix3 -> Int
botsInRange bots p = length $ filter (\b -> inRange b p) bots


neighbours :: Ix3 -> [Ix3]
neighbours (x :> y :. z) = 
    [ (x+1) :> y :. z
    , (x-1) :> y :. z
    , x :> (y+1) :. z
    , x :> (y-1) :. z
    , x :> y :. (z+1)
    , x :> y :. (z-1)]


minWith :: Ord b => (a -> b) -> [a] -> a
minWith f ls = foldl1' (\x y -> if f x <= f y then x else y) ls

maxWith :: Ord b => (a -> b) -> [a] -> a
maxWith f ls = foldl1' (\x y -> if f x >= f y then x else y) ls


-- part b the real deal

octize :: Octant -> [Octant]
octize (Octant (lx:>ly:.lz) (ux:>uy:.uz)) = 
    [ Octant (lx :> ly :. lz) (mx :> my :. mz)
    , Octant (mx :> ly :. lz) (ux :> my :. mz)
    , Octant (lx :> my :. lz) (mx :> uy :. mz)
    , Octant (lx :> ly :. mz) (mx :> my :. uz)
    , Octant (mx :> my :. lz) (ux :> uy :. mz)
    , Octant (mx :> ly :. mz) (ux :> my :. uz)
    , Octant (lx :> my :. mz) (mx :> uy :. uz)
    , Octant (mx :> my :. mz) (ux :> uy :. uz)]
    where
        mx = div (ux + lx) 2
        my = div (uy + ly) 2
        mz = div (uz + lz) 2


boxSearch :: [Nanobot] -> Octant -> Ix3
boxSearch bots box = maxWith (\p -> (botsInRange bots p, - (manhattan (0:>0:.0) p))) points where
    (lx :> ly :. lz) = lower box
    (ux :> uy :. uz) = upper box
    points = [(x :> y :. z) | x <- [lx..ux]
                            , y <- [ly..uy]
                            , z <- [lz..uz]]


adaptiveBound :: [Nanobot] -> Int
adaptiveBound bots = next2Power $ (maximum $ map coordMax bots) + (maximum $ map fst bots)
    where coordMax :: Nanobot -> Int
          coordMax (_, (x:>y:.z)) = maximum $ map abs [x, y, z]


next2Power :: Int -> Int
next2Power n = shiftL 1 (go n 0) where
    go :: Int -> Int -> Int
    go 0 c = c
    go n c = go (shiftR n 1) (c+1)


-- random point generation

randomPoint :: Octant -> IO Ix3
randomPoint (Octant (lx:>ly:.lz) (ux:>uy:.uz)) = do
    rx <- uniformRM (lx, ux) globalStdGen :: IO Int
    ry <- uniformRM (ly, uy) globalStdGen :: IO Int
    rz <- uniformRM (lz, uz) globalStdGen :: IO Int
    return (rx :> ry :. rz)


randomPointList :: Int -> Octant -> IO [Ix3]
randomPointList n oct = mapM randomPoint (take n $ repeat oct)


-- workhorse function
-- given a list of bots
--      find the adaptive range, start as off with a -that, +that octant
--      while octant size > 64
--          octize, and for each octant
--              take some big list of points (maybe adaptively sized)
--              find botsInRange for each point
--              find max of those botsInRange
--          pick the octant with highest max botsInRange
--      when octant size <= 64 run a boxSearch on the octant, return that
randomSearch :: [Nanobot] -> IO Ix3
randomSearch bots = do
    let b = div (adaptiveBound bots) 8
    let octant = Octant (-b :> -b :. -b) (b :> b :. b)
    ix <- randomHelper bots octant
    return ix


randomHelper :: [Nanobot] -> Octant -> IO Ix3
randomHelper bots octant = do
    if width octant <= 32
        then do
            putStrLn "Beginning box search"
            return $ boxSearch bots octant
        else do 
            let octs = octize octant
            pointsLs <- mapM (randomPointList 8192) octs
            let records = map (\ls -> maximum $ map (botsInRange bots) ls) pointsLs
            print records
            let (r, oct) = maxWith (\x -> (fst x, -1 * (distToOrigin $ snd x))) $ zip records octs
            putStrLn $ "Record of " ++ show r ++ " @ width " ++ show (width oct)
            ix <- randomHelper bots oct
            return ix


distToOrigin :: Octant -> Int
distToOrigin (Octant (lx:>ly:.lz) (ux:>uy:.uz)) = manhattan (0:>0:.0) (mx:>my:.mz) where
    mx = div (ux + lx) 2
    my = div (uy + ly) 2
    mz = div (uz + lz) 2


width :: Octant -> Int
width (Octant (lx:>ly:.lz) (ux:>uy:.uz)) = maximum [ux-lx, uy-ly, uz-lz]


center :: Octant -> Ix3
center (Octant (lx:>ly:.lz) (ux:>uy:.uz)) = (mx:>my:.mz) where
    mx = div (ux + lx) 2
    my = div (uy + ly) 2
    mz = div (uz + lz) 2

-- proxy for checking actual intersection
octInRangeProxy :: Nanobot -> Octant -> Bool
octInRangeProxy (r, p) oct = dist <= r' + dl where
    c = center oct
    dist = fromIntegral $ manhattan p c
    r' = fromIntegral $ r
    dl = (fromIntegral $ width oct) * ((sqrt 3) / 2) :: Double


octInRange :: Nanobot -> Octant -> Bool
octInRange (r, p) (Octant l u) = manhattan c p <= r where
    (px:>py:.pz) = p
    (lx:>ly:.lz) = l
    (ux:>uy:.uz) = u
    cx = clamp px lx ux
    cy = clamp py ly uy
    cz = clamp pz lz uz
    c = (cx:>cy:.cz)


numInRange :: [Nanobot] -> Octant -> Int
numInRange bots octant = length $ filter (\b -> octInRange b octant) bots


clamp :: Int -> Int -> Int -> Int
clamp x low high
    | x < low               = low
    | low <= x && x <= high = x
    | high < x              = high


-- solution based on testing octants to see how many ranges they intersect
octFilter :: [Nanobot] -> IO Ix3
octFilter bots = octFilterHelper bots octant where
    b = div (adaptiveBound bots) 2
    octant = Octant (-b :> -b :. -b) (b :> b :. b)


octFilterHelper :: [Nanobot] -> Octant -> IO Ix3
octFilterHelper bots octant = do
    if width octant <= 32
        then do
            print "Beginning box search"
            return $ boxSearch bots octant
        else do
            let octs = octize octant
            let records = map (numInRange bots) octs
            print records
            let (r, oct) = maxWith (\x -> (fst x, -1 * (distToOrigin $ snd x))) $ zip records octs
            putStrLn $ "Record of " ++ show r ++ " @ width " ++ show (width oct)
            ix <- octFilterHelper bots oct
            return ix


-- better testing of ranges, also queue of ranges to try

octQueuer :: [Nanobot] -> IO Ix3
octQueuer bots = octQueuerHelper bots (S.singleton (bir, octant)) (0:>0:.0) where
    b = div (adaptiveBound bots) 2
    octant = Octant (-b :> -b :. -b) (b :> b :. b)
    bir = numInRange bots octant


octQueuerHelper :: [Nanobot] -> Set (Int, Octant) -> Ix3 -> IO Ix3
octQueuerHelper bots queue leader = do
    let ((octCount, oct), queue') = S.deleteFindMax queue
    print $ "Examining octant with rangeCount of " ++ show octCount ++ " and width " ++ (show $ width oct)
    let leaderCount = botsInRange bots leader
    if leaderCount > octCount
        then return leader
        else do 
            if width oct <= 2
                then do
                    putStrLn "Box searching for a new leader"
                    let boxLeader = boxSearch bots oct
                    let newLeader = maximumBy (pickLeader bots) [leader, boxLeader]
                    octQueuerHelper bots queue' newLeader
                else do
                    putStrLn "Splitting octants and queuing them up"
                    let octs = octize oct
                    let rangeCounts = map (numInRange bots) octs
                    let biggerQueue = S.union (S.fromList $ zip rangeCounts octs) queue'
                    octQueuerHelper bots biggerQueue leader


pickLeader :: [Nanobot] -> Ix3 -> Ix3 -> Ordering
pickLeader bots p1 p2
    | bir1 /= bir2      = compare bir1 bir2
    | dto1 /= dto2      = compare dto2 dto1 -- flipped so that smaller
    | otherwise         = EQ
      where
        bir1 = botsInRange bots p1
        bir2 = botsInRange bots p2
        dto1 = manhattan (0:>0:.0) p1
        dto2 = manhattan (0:>0:.0) p2

    

-- mains

mainA :: IO ()
mainA = do
    nanobots <- parseInput "input.txt"
    let answer = inRangeOfMax nanobots
    print answer
    -- result <- submitAnswer 2018 23 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    bots <- parseInput "input.txt"
    answerIX <- octQueuer bots
    print answerIX
    print $ botsInRange bots answerIX
    let answer = manhattan answerIX (0:>0:.0)
    print answer
    -- result <- submitAnswer 2018 23 2 answer
    -- print result
    return ()
