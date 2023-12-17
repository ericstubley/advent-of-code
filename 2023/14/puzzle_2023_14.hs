module Puzzle_2023_14 where

import Automation (submitAnswer)
import Parsing
import Data.Massiv.Array (Array, Ix2(..), Sz(..), (<!), (!>))
import qualified Data.Massiv.Array as A
import Data.Sort (sortOn)
import Data.List (transpose)
import Control.Monad.State
import Control.Applicative (liftA2)
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as M

-- data types
type Array2 a = A.Array A.U A.Ix2 a
type Platform = Array2 Char
type Pform = [[Char]]

-- parsing
platformP :: Parser Platform
platformP = A.fromLists' A.Seq <$> sepBy (some rockP) newline

pformP :: Parser Pform
pformP = columns <$> platformP

rockP :: Parser Char
rockP = char '.' <|> char 'O' <|> char '#'

-- functions
columns :: Platform -> [[Char]]
columns platform = A.toLists2 . A.transpose $ platform


collect :: [Char] -> Int
collect line = collect' len line
  where len = length line
        collect' i [] = 0
        collect' i xs@(x:xs')
            | x == '#'  = collect' (i-1) xs'
            | otherwise = n + collect' (i-j) end
              where (rolls, end) = span (/= '#') xs
                    c = length . filter (== 'O') $ rolls
                    j = length rolls
                    n = c*i  - (sum [1..(c-1)])


order :: [Char] -> [Char]
order [] = []
order xs@(x:xs')
    | x == '#'  = x:(order xs')
    | otherwise = (sortOn sorter) rolls ++ order end
      where (rolls, end) = span (/= '#') xs


sorter :: Char -> Int
sorter c
    | c == 'O' = 0
    | c == '.' = 1


run :: MonadState Pform m => m ()
run = tilt >> rotate >> tilt >> rotate >> tilt >> rotate >> tilt >> rotate

tilt :: MonadState Pform m => m ()
tilt = modify (map order)

rotate :: MonadState Pform m => m ()
rotate = modify (reverse . transpose)


unrotate :: MonadState Pform m => m ()
unrotate = modify (transpose . reverse)


-- if current state in map of previously seen states, then return (i, j)
-- if not, add, run cycle, then recurse
detect :: MonadState Pform m => m (Int, Int)
detect = detect' 0 M.empty
  where detect' :: MonadState Pform m => Int -> Map Pform Int -> m (Int, Int)
        detect' j prev = do
            s <- get
            if (M.member s prev)
                then return (prev M.! s, j)
                else do
                    let prev' = M.insert s j prev
                    run
                    detect' (j+1) prev'


repeatM :: (Applicative m) => Int -> m a -> m [a]
repeatM n a = loop n
    where loop n
            | n <= 0    = pure []
            | otherwise = liftA2 (:) a (loop (n - 1))


load :: [Char] -> Int
load line = load' len line
  where len = length line
        load' :: Int -> [Char] -> Int 
        load' i [] = 0
        load' i (x:xs')
            | x == 'O'  = i + (load' (i-1) xs')
            | otherwise = load' (i-1) xs'

stressTestA :: Pform -> Int
stressTestA pform = sum . map load $ pform'
  where pform' = execState (modify (map order)) pform


stressTestB :: Int -> Pform -> Int
stressTestB n pform = sum . map load $ pform'
  where (i, j) = evalState detect pform
        k = mod (n-i) (j-i)
        pform' = last $ evalState (repeatM (i+k) (run >> get)) pform




-- mains

mainA :: IO ()
mainA = do
    (Just pform) <- parseInput pformP "14/input.txt"
    let answer = stressTestA pform
    print answer
    -- result <- submitAnswer 2023 14 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just pform) <- parseInput pformP "14/input.txt"
    let answer = stressTestB 1000000000 pform
    print answer
    -- result <- submitAnswer 2023 14 2 answer
    -- print result
    return ()
