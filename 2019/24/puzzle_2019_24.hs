module Puzzle_2019_24 where

import Automation (submitAnswer)
import Parsing
import Data.Bits
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- data types
type Bugs = Int -- synonym for our bit usage of ints



-- parsing

bugsP :: Parser Int
bugsP = do
    lls <- sepBy (count 5 bugP) newline
    return $ readBugs lls


bugP :: Parser Char
bugP = char '.' <|> char '#'


showBugs :: Bugs -> [[Char]]
showBugs bugs = chunksOf 5 (pad 25 '.' display)
  where pad :: Int -> a -> [a] -> [a]
        pad n fill ls = take n (ls ++ repeat fill)
        display = map (\b -> if b == 1 then '#' else '.') (bitList bugs)


printBugs :: Bugs -> IO ()
printBugs bugs = mapM_ putStrLn (showBugs bugs)


readBugs :: [[Char]] -> Bugs
readBugs lls = sum $ zipWith (*) bits powersOfTwo
  where bits = map (\c -> if c == '#' then 1 else 0) (concat lls)


-- functions
bitList :: Bugs -> [Int]
bitList 0 = []
bitList n = (mod n 2) : bitList (div n 2)


chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n ls = (take n ls) : chunksOf n (drop n ls)


powersOfTwo :: [Int]
powersOfTwo = map (shiftL 1) [0..]


cutoff :: Bugs
cutoff = sum $ take 25 powersOfTwo


left :: Bugs -> Bugs
left bugs = shiftL bugs' 1
  where bugs' = foldl (\acc x -> clearBit acc x) bugs [4, 9, 14, 19, 24]


right :: Bugs -> Bugs
right bugs = shiftR bugs' 1
  where bugs' = foldl (\acc x -> clearBit acc x) bugs [0, 5, 10, 15, 20]


above :: Bugs -> Bugs
above bugs = shiftR bugs 5


below :: Bugs -> Bugs
below bugs = (shiftL bugs 5) .&. cutoff


oneNeighbour :: Bugs -> Bugs
oneNeighbour bugs = cutoff .&. oddnbs .&. (complement any3)
  where l = left  bugs
        r = right bugs
        a = above bugs
        b = below bugs
        oddnbs = l `xor` r `xor` a `xor` b
        any3 =  (l .&. r .&. a) 
            .|. (l .&. r .&. b) 
            .|. (l .&. a .&. b) 
            .|. (r .&. a .&. b)


twoNeighbours :: Bugs -> Bugs
twoNeighbours bugs = cutoff .&. ge1 .&. evennbs .&. not4
  where l = left  bugs
        r = right bugs
        a = above bugs
        b = below bugs
        evennbs = complement (l `xor` r `xor` a `xor` b)
        ge1 = l .|. r .|. a .|. b
        not4 = complement (l .&. r .&. a .&. b)


step :: Bugs -> Bugs
step bugs = one .|. ((complement bugs) .&. two)
  where one = oneNeighbour  bugs
        two = twoNeighbours bugs


firstRepeat :: Bugs -> Bugs
firstRepeat bugs = go bugs S.empty where
    go bugs seen
        | S.member bugs seen = bugs
        | otherwise          = go (step bugs) (S.insert bugs seen)


-- part b functions which are totally different???

data Loc = Loc Int Int Int deriving (Eq, Ord, Show)
type BugMap = Map Loc Bool

-- parsing

mapP :: Parser BugMap
mapP = do
    lls <- sepBy (count 5 bugP) newline
    let lls' = concat lls
    let lls'' = map (\x -> if x == '#' then True else False)
                   $ (take 12 lls') ++ (drop 13 lls')
    return $ M.fromList (zip (blankLocs 0) lls'')


blankLocs :: Int -> [Loc]
blankLocs level = [Loc i j level | i <- [0..4], j <- [0..4]
                                  , (i, j) /= (2, 2)]


blankLevel :: Int -> BugMap
blankLevel level = M.fromList $ zip (blankLocs level) (repeat False)


adjacent :: Loc -> [Loc]
adjacent (Loc i j l) = le ++ ri ++ up ++ dw
  where le = case (i, j) of
                (2, 3) -> [Loc a 4 (l+1) | a <- [0..4]]
                (_, 0) -> [Loc 2 1 (l-1)]
                (_, _) -> [Loc i (j-1) l]
        ri = case (i, j) of
                (2, 1) -> [Loc a 0 (l+1) | a <- [0..4]]
                (_, 4) -> [Loc 2 3 (l-1)]
                (_, _) -> [Loc i (j+1) l]
        up = case (i, j) of
                (3, 2) -> [Loc 4 a (l+1) | a <- [0..4]]
                (0, _) -> [Loc 1 2 (l-1)]
                (_, _) -> [Loc (i-1) j l]
        dw = case (i, j) of
                (1, 2) -> [Loc 0 a (l+1) | a <- [0..4]]
                (4, _) -> [Loc 3 2 (l-1)]
                (_, _) -> [Loc (i+1) j l]


neighbours :: BugMap -> Loc -> [Loc]
neighbours bugs loc = filter counter (adjacent loc)
  where counter l = M.findWithDefault False l bugs


stepMap :: Int -> BugMap -> BugMap
stepMap s bugs = M.mapWithKey (stepCell bugs) bugs'
  where s' = div (s+1) 2
        bugs' = if odd s
                    then M.unions [bugs, blankLevel s', blankLevel (-s')]
                    else bugs


stepCell :: BugMap -> Loc -> Bool -> Bool
stepCell bugs loc bug
    | nbs == 1              = True
    | (not bug) && nbs == 2 = True
    | otherwise             = False
      where nbs = length (neighbours bugs loc)


countBugs :: BugMap -> Int
countBugs bugs = length . filter (id) $ M.elems bugs


evolve :: Int -> BugMap -> BugMap
evolve n bugs = go 1 bugs
  where go :: Int -> BugMap -> BugMap
        go t bugs
            | t <= n    = go (t+1) (stepMap t bugs)
            | otherwise = bugs


-- mains

mainA :: IO ()
mainA = do
    (Just bugs) <- parseInput bugsP "24/input.txt"
    let answer = firstRepeat bugs
    print answer
    -- result <- submitAnswer 2019 24 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just bugs) <- parseInput mapP "24/input.txt"
    let answer = countBugs (evolve 200 bugs)
    print answer
    -- result <- submitAnswer 2019 24 2 answer
    -- print result
    return ()
