module Puzzle_2023_12 where

import Automation (submitAnswer)
import Parsing
import Control.Applicative (liftA2)
import Control.Monad (filterM)
import Control.Monad.State.Lazy
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as M
import Data.Maybe

-- data types
data Spring = Operational | Damaged | Unknown deriving (Eq, Ord, Show)
data Report = Report { _row :: [Spring]
                     , _dup :: [Int] } deriving (Eq, Ord, Show)

type Memo = Map Report Bool
type MemoI = Map Report Int

-- parsing
reportsP :: Parser [Report]
reportsP = sepBy reportP newline

reportP :: Parser Report
reportP = do
    row <- many springP
    char ' '
    dup <- sepBy natural (char ',')
    return $ Report row dup

springP :: Parser Spring
springP = (char '.' >> return Operational)
      <|> (char '#' >> return Damaged)
      <|> (char '?' >> return Unknown)

-- functions
countArrangements :: Report -> Int
countArrangements report = length . options $ report 

options :: Report -> [Report]
options report = evalState (memoSearch report) initial
  where initial = M.singleton (Report [] []) True

memoSearch :: MonadState Memo m => Report -> m [Report]
memoSearch (Report row dup) = filterM tester $ map (\r -> Report r dup) . possibilities $ row

tester :: MonadState Memo m => Report -> m Bool
tester report = do
    memoized <- gets (M.member report) 
    if memoized
        then gets (\s -> s M.! report)
        else do
            let r = consume report
            if isNothing r
                then do
                    modify (M.insert report False)
                    return False
                else do
                    let report' = fromJust r
                    result <- tester report'
                    modify (M.insert report result)
                    return result

-- should never get to the case where report is totally empty
-- if head is damaged and count matches, you're good
-- if head is operational just drop them all
-- any other case is bad
consume :: Report -> Maybe Report
consume (Report ss@(Damaged:_) (d:ds))
    | length (takeWhile (==Damaged) ss) == d = Just (Report (dropWhile (==Damaged) ss) ds)
    | otherwise                              = Nothing
consume (Report (Operational:ss) ds) = Just (Report (dropWhile (==Operational) ss) ds)
consume _ = Nothing

possibilities :: [Spring] -> [[Spring]]
possibilities (x:[])
    | x == Unknown     = [[Operational], [Damaged]]
    | otherwise        = [[x]]
possibilities (x:xs)
    | x == Unknown     = map (\ls -> Operational:ls) xs' ++ map (\ls -> Damaged:ls) xs'
    | otherwise        = map (\ls -> x:ls) xs'
      where xs' = possibilities xs

printer :: Report -> String
printer report = map p $ _row report
  where p Operational = '.'
        p Damaged = '#'
        p Unknown = '?'

unfold :: Report -> Report
unfold (Report row dup) = Report row' dup'
  where row' = tail . take (5 * (1 + length row)) . cycle $ (Unknown:row)
        dup' = take (5 * (length dup)) . cycle $ dup

counter :: MonadState MemoI m => Report -> m Int
counter report = do
    memoized <- gets (M.member report)
    if memoized
        then gets (\s -> s M.! report)
        else do
            n <- counter' report
            modify (M.insert report n)
            return n

-- no memo lookup version
-- if you start with Unknown, try both paths
-- if you start with Operational, drop all
-- if you start with Damaged, test for right number of Damaged/Unknown + Operational or Unknown you can force
-- avoid that 0 because that messes you up
counter' :: MonadState MemoI m => Report -> m Int
counter' (Report ss@(Unknown:ss') ds) = liftA2 (+) (counter (Report (Operational:ss') ds)) (counter (Report (Damaged:ss') ds))
counter' (Report ss@(Operational:ss') ds) = counter (Report (dropWhile (==Operational) ss') ds)
counter' (Report ss@(Damaged:ss') []) = return 0
counter' (Report ss@(Damaged:ss') ds@(d:ds'))
    | correctPrefix && emptySuffix = counter (Report [] ds')
    | correctPrefix && fullSuffix  = counter (Report (drop d ss') ds')
    | otherwise = return 0
      where (prefix, suffix) = splitAt d ss
            correctPrefix = length prefix == d && all (\x -> x == Damaged || x == Unknown) prefix
            emptySuffix = length suffix == 0
            fullSuffix = length suffix > 0 && (head suffix == Unknown || head suffix == Operational)
counter' (Report [] ds@(d:ds')) = return 0

countArrangements' :: Report -> Int
countArrangements' report = evalState (counter report) initial
  where initial = M.singleton (Report [] []) 1


-- mains

mainA :: IO ()
mainA = do
    (Just reports) <- parseInput reportsP "12/input.txt"
    let answer = sum . map countArrangements' $ reports
    print answer
    -- result <- submitAnswer 2023 12 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just reports) <- parseInput reportsP "12/input.txt"
    let answer = sum . map (countArrangements' . unfold) $ reports
    print answer
    result <- submitAnswer 2023 12 2 answer
    print result
    return ()
