module Puzzle_2023_05 where

import Automation (submitAnswer)
import Parsing

-- data types
newtype Piecewise = Piecewise {getPiecewise :: Int -> Int}

instance Semigroup Piecewise where
    (Piecewise f) <> (Piecewise g) = Piecewise merge where
        merge n
            | f(n) /= n = f(n)
            | g(n) /= n = g(n)
            | otherwise = n

instance Monoid Piecewise where
    mempty = Piecewise id

newtype Almanac = Almanac {getAlmanac :: Int -> Int}

instance Semigroup Almanac where
    (Almanac f) <> (Almanac g) = Almanac (g . f)

instance Monoid Almanac where
    mempty = Almanac id

data Interval = Interval { _start :: Int 
                         , _width :: Int } deriving (Eq, Ord, Show)

data Builder = Builder { _destination :: Int
                       , _interval :: Interval } deriving (Eq, Ord, Show)

-- parsing
inputP :: Parser ([Int], Int -> Int)
inputP = do
    seeds <- seedsP
    newline
    newline
    almanac <- getAlmanac <$> mconcat <$> sepBy almanacP newline
    return (seeds, almanac)

seedsP :: Parser [Int]
seedsP = string "seeds:" *> many (hspace *> natural)

almanacP :: Parser Almanac
almanacP = do
    many (lowerChar <|> char '-')
    space
    string "map:"
    newline
    piecewise <- mconcat <$> sepEndBy piecewiseP newline
    return $ Almanac (getPiecewise piecewise)

piecewiseP :: Parser Piecewise
piecewiseP = do
    a <- natural
    b <- hspace >> natural
    c <- hspace >> natural
    return $ Piecewise (builder a b c)

inputBP :: Parser ([Interval], [[Builder]])
inputBP = do
    seeds <- seedRanges <$> seedsP
    newline
    newline
    almanac <- sepBy almanacBP newline
    return (seeds, almanac)

almanacBP :: Parser [Builder]
almanacBP = do
    many (lowerChar <|> char '-')
    space
    string "map:"
    newline
    builders <- sepEndBy builderP newline
    return builders

builderP :: Parser Builder
builderP = do
    a <- natural
    b <- hspace >> natural
    c <- hspace >> natural
    return $ Builder a (Interval b c)

-- functions
builder :: Int -> Int -> Int -> Int -> Int
builder destination source range n
    | offset >= 0 && offset < range = destination + offset
    | otherwise = n
      where offset = n - source

seedList :: [Int] -> [Int]
seedList [] = []
seedList (start:range:seeds) = [start..(start + range - 1)] ++ (seedList seeds)


seedRanges :: [Int] -> [Interval]
seedRanges [] = []
seedRanges (start:range:seeds) = Interval start range : (seedRanges seeds)

bigBrain :: [Int] -> (Int -> Int) -> Int
bigBrain seeds locator = minimum $ map locator (seedList seeds)


process :: [Interval] -> [[Builder]] -> [Interval]
process seeds stages = foldl apply seeds stages


apply :: [Interval] -> [Builder] -> [Interval]
apply ranges stage = concat $ map (push stage) ranges


-- ooooof ugly, what a constrast with part a
push :: [Builder] -> Interval -> [Interval]
push [] j = [j]
push (f:fs) j
    | o && a && b = [o'] ++ (push fs a') ++ (push fs b')
    | o && a      = o' : (push fs a')
    | o && b      = o' : (push fs b')
    | o           = [o']
    | otherwise   = push fs j
      where i = _interval f
            o = overlap i j
            int = interior i j
            o' = Interval (_destination f + (_start int) - (_start i)) (_width int)
            a = above i j
            a' = sequel i j
            b = below i j
            b' = prequel i j


-- do two intervals overlap?
overlap :: Interval -> Interval -> Bool
overlap (Interval a w) (Interval a' w') 
    = (a <= a' && a' < (a+w)) || (a' <= a && a < (a' + w'))

-- does second arg have a piece above first arg?
above :: Interval -> Interval -> Bool
above (Interval a w) (Interval a' w') = (a+w) < (a'+w')

-- does second arg have a piece below first arg?
below :: Interval -> Interval -> Bool
below (Interval a w) (Interval a' w') = a' < a

-- assuming first arg overlaps second arg, get the intersection
interior :: Interval -> Interval -> Interval
interior (Interval a w) (Interval a' w') = Interval s r
  where s = max a a'
        r = (min (a+w) (a'+w')) - s

-- get the part that is below, given that they overlap
prequel :: Interval -> Interval -> Interval
prequel (Interval a w) (Interval a' w') = Interval a' (a-a')

-- get the part that is above, given that they overlap
sequel :: Interval -> Interval -> Interval
sequel (Interval a w) (Interval a' w') = Interval (a+w) (a'+w'-a-w)

-- mains

mainA :: IO ()
mainA = do
    (Just (seeds, locator)) <- parseInput inputP "05/input.txt"
    let answer = minimum $ map locator seeds
    print answer
    -- result <- submitAnswer 2023 05 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just (seeds, stages)) <- parseInput inputBP "05/input.txt"
    let answer = minimum . (map _start) $ process seeds stages
    print answer
    -- result <- submitAnswer 2023 05 2 answer
    -- print result
    return ()
