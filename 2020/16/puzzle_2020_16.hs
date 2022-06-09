module Puzzle_2020_16 where

import Automation (submitAnswer)
import Parsing
import Data.Set (Set)
import qualified Data.Set as S

-- data types
-- captures the range [lower1, upper1] \cup [lower2, upper2]
data Range = Range
    { name :: String
    , lower1 :: Int
    , upper1 :: Int
    , lower2 :: Int
    , upper2 :: Int}
    deriving (Eq, Ord, Show)

type Ticket = [Int]

-- parsing
notesP :: Parser ([Range], Ticket, [Ticket])
notesP = do
    ranges <- some (rangeP <* optional newline)
    newline >> string "your ticket:" >> newline
    yourTicket <- ticketP
    newline >> newline >> string "nearby tickets:" >> newline
    tickets <- some (ticketP <* optional newline)
    return (ranges, yourTicket, tickets)


rangeP :: Parser Range
rangeP = do
    name <- some (lowerChar <|> char ' ')
    string ": "
    (l1, u1) <- intervalP
    string " or "
    (l2, u2) <- intervalP
    return $ Range name l1 u1 l2 u2


intervalP :: Parser (Int, Int)
intervalP = do
    l <- integer
    char '-'
    u <- integer
    return (l, u)


ticketP :: Parser Ticket
ticketP = some (integer <* optional (char ','))

-- functions
-- True if there's no range for which value is inRange
invalid :: [Range] -> Int -> Bool
invalid ranges value = all (\r -> not $ inRange r value) ranges

-- True if there is a range for which value is inRange
valid :: [Range] -> Int -> Bool
valid ranges value = not $ invalid ranges value


-- True if value is in the intervals given by range
inRange :: Range -> Int -> Bool
inRange (Range _ a b c d) value = 
       (a <= value && value <= b)
    || (c <= value && value <= d)


scanningErrorRate :: [Range] -> [Ticket] -> Int
scanningErrorRate ranges tickets = sum $ filter (invalid ranges) (concat tickets)


-- True if every value of ticket is valid
validTicket :: [Range] -> Ticket -> Bool
validTicket ranges ticket = all (valid ranges) ticket


-- True if every value in the list is inRange
-- think of the list of values as all the n-th elements of the tickets
possibleRange :: Range -> [Int] -> Bool
possibleRange range position = all (inRange range) position


rangesForPosition :: [Range] -> [Ticket] -> Int -> [Range]
rangesForPosition ranges tickets i = filter (\r -> possibleRange r position) ranges
  where position = map (\t -> t !! i) tickets


-- ranges is sorted to the same field ordering as ticket itself
departureProduct :: [Range] -> Ticket -> Int
departureProduct ranges ticket = foldl selector 1 (zip ticket ranges)
  where selector :: Int -> (Int, Range) -> Int
        selector acc (value, range)
            | take 9 (name range) == "departure" = acc*value
            | otherwise                          = acc


identifyFields :: [Range] -> [Ticket] -> [Range]
identifyFields ranges tickets = map head (go possibleRanges)
  where validTickets = filter (validTicket ranges) tickets
        possibleRanges = map (rangesForPosition ranges validTickets) [0..((length ranges)-1)]
        go :: [[Range]] -> [[Range]]
        go possibilities
            | all (\ps -> length ps == 1) possibilities = possibilities
            | otherwise = go $ map (simplify toRemove) possibilities
              where toRemove = concat $ filter (\ps -> length ps == 1) possibilities


-- any list which has only 1 range stays the same
-- otherwise, take out everything from toRemove
simplify :: [Range] -> [Range] -> [Range] 
simplify toRemove toSimplify
    | length toSimplify == 1 = toSimplify
    | otherwise              = filter (\r -> not $ elem r toRemove) toSimplify


-- mains

mainA :: IO ()
mainA = do
    (Just (ranges, _, tickets)) <- parseInput notesP "16/input.txt"
    let answer = scanningErrorRate ranges tickets
    print answer
    -- result <- submitAnswer 2020 16 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just (ranges, ticket, tickets)) <- parseInput notesP "16/input.txt"
    let fields = identifyFields ranges (ticket:tickets)
    let answer = departureProduct fields ticket
    print answer
    -- result <- submitAnswer 2020 16 2 answer
    -- print result
    return ()
