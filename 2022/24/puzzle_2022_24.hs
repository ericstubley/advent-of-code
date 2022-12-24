module Puzzle_2022_24 where

import Automation (submitAnswer)
import Parsing
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.RWS
import Data.Vector (Vector)
import qualified Data.Vector as V

-- data types
data Direction = North | East | South | West deriving (Eq, Ord, Show)
data Wind = Boreal | Vultur | Auster | Zephyr | Still deriving (Eq, Ord, Show)

data Blizzard = Blizzard
    { variety :: Wind
    , majorIndex :: Int
    , minorIndex :: Int -> Int}

data Storm = Storm
    { height :: Int
    , width  :: Int
    , boreals :: Vector [Blizzard]
    , vulturs :: Vector [Blizzard]
    , austers :: Vector [Blizzard]
    , zephyrs :: Vector [Blizzard]}

-- parsing
stormP :: Parser Storm
stormP = do
    rows <- between topP bottomP (some rowP)
    return $ buildStorm rows


topP :: Parser ()
topP = do
    char '#'
    char '.'
    some (char '#')
    newline
    char '#'
    return ()


bottomP :: Parser ()
bottomP = do
    some (char '#')
    char '.'
    char '#'
    return ()


rowP :: Parser [Wind]
rowP = some groundP <* (char '#' >> newline >> char '#')


groundP :: Parser Wind
groundP = (char '^' >> return Auster)
      <|> (char '>' >> return Zephyr)
      <|> (char 'v' >> return Boreal)
      <|> (char '<' >> return Vultur)
      <|> (char '.' >> return Still)


buildStorm :: [[Wind]] -> Storm
buildStorm winds = Storm cols rows nv ev sv wv where
    rows = length winds
    cols = length (winds !! 0)
    indexedColumns = map (zip [0..]) winds
    indexedWinds = concat $ zipWith (\r col -> map (\t -> ((r, fst t), snd t)) col) [0..] indexedColumns
    blizzards = map builder . filter ((/= Still) . snd) $ indexedWinds
    builder :: ((Int, Int), Wind) -> Blizzard
    builder ((r, c), w) = case w of
                            Boreal -> Blizzard Boreal c (forecast cols r 1)
                            Vultur -> Blizzard Vultur r (forecast rows c (-1))
                            Auster -> Blizzard Auster c (forecast cols r (-1))
                            Zephyr -> Blizzard Zephyr r (forecast rows c 1)
    ns = filter ((== Boreal) . variety) blizzards
    es = filter ((== Vultur) . variety) blizzards
    ss = filter ((== Auster) . variety) blizzards
    ws = filter ((== Zephyr) . variety) blizzards
    nv = V.fromList $ map (\x -> filter ((== x) . majorIndex) ns) [0..(cols-1)]
    ev = V.fromList $ map (\x -> filter ((== x) . majorIndex) es) [0..(rows-1)]
    sv = V.fromList $ map (\x -> filter ((== x) . majorIndex) ss) [0..(cols-1)]
    wv = V.fromList $ map (\x -> filter ((== x) . majorIndex) ws) [0..(rows-1)]


forecast :: Int -> Int -> Int -> Int -> Int
forecast wrap start sign = \t -> mod (start + sign*t) wrap
        

-- functions

findGoal :: Storm -> Int
findGoal storm = fst $ execRWS (pathfinder 1 (0, 0)) storm (maxBound :: Int)


-- if t >= best, return right away
-- otherwise check if at exit, update if better time
-- otherwise
pathfinder :: MonadRWS Storm () Int m => Int -> (Int, Int) -> m ()
pathfinder t (r, c) = do
    best <- get
    if t >= best then return ()
    else do
        exit <- atExit (r, c) 
        if exit then update (t+1)
        else do
            options <- filterM inValley $ neighbours (r, c)
            options' <- filterM (open (t+1)) $ options
            if length options' == 0 then return ()
                else do
                    mapM_ (pathfinder (t+1)) options'


update :: MonadState Int m => Int -> m ()
update t = do
    best <- get
    if t < best
        then put t
        else return ()


neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (r, c) = [(r+1, c), (r, c+1), (r, c), (r-1, c), (r, c-1)]


atExit :: MonadReader Storm m => (Int, Int) -> m Bool
atExit (r, c) = do
    rows <- reader height
    cols <- reader width
    return $ r == (rows-1) && c == (cols-1)


inValley :: MonadReader Storm m => (Int, Int) -> m Bool
inValley (r, c) = do
    rows <- reader height
    cols <- reader width
    return $ 0 <= r && r < rows && 0 <= c && c < cols


open :: MonadReader Storm m => Int -> (Int, Int) -> m Bool
open t (r, c) = do
    ns <- reader $ (V.! c) . boreals
    es <- reader $ (V.! r) . vulturs
    ss <- reader $ (V.! c) . austers
    ws <- reader $ (V.! r) . zephyrs
    let no = all (/=r) $ map (\b -> (minorIndex b) t) ns
    let eo = all (/=c) $ map (\b -> (minorIndex b) t) ns
    let so = all (/=r) $ map (\b -> (minorIndex b) t) ns
    let wo = all (/=c) $ map (\b -> (minorIndex b) t) ns
    return $ and [no, eo, so, wo]

-- mains

mainA :: IO ()
mainA = do
    let answer = 0
    print answer
    -- result <- submitAnswer 2022 24 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    let answer = 0
    print answer
    -- result <- submitAnswer 2022 24 2 answer
    -- print result
    return ()
