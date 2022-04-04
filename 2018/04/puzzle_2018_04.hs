module Puzzle_2018_04 where

-- imports
import Data.List (elemIndex, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Sort
import Data.Time
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import System.IO
import Automation (submitAnswer)


-- data types
type Parser = Parsec Void String

data Status = OnGuard | Awake | Asleep deriving (Show, Eq)

data WallEntry = WallEntry {
    time :: UTCTime,
    minute :: Int,
    ident :: Maybe Int,
    status :: Status } deriving (Show, Eq)

-- parsing
parseInput :: String -> IO [WallEntry]
parseInput filename = do
    raw <- readFile filename
    -- let (Right temp) = runParser lineParser "" "[1518-11-01 00:25] wakes up"
    -- print $ show temp
    let (Right ws) = runParser linesParser "" raw
    let properWs = assignIDs $ sortOn (time) ws
    return properWs


linesParser :: Parser [WallEntry]
linesParser = lineParser `sepBy` char '\n'


lineParser :: Parser WallEntry
lineParser = do
    (t, minute) <- timeParser
    (guardID, guardStatus) <- choice [beginParser, asleepParser, wakeParser]
    return $ WallEntry t minute guardID guardStatus

-- [1518-11-01 00:00] Guard #10 begins shift
-- [1518-11-01 00:05] falls asleep
-- [1518-11-01 00:25] wakes up

timeParser :: Parser (UTCTime, Int)
timeParser = do
    char '['
    timeString <- count 16 (char ':' <|> char '-' <|> char ' ' <|> digitChar)
    char ']'
    char ' '
    let processed = timeString ++ ":00 UTC"
    let t = read processed
    let minute = read $ drop 14 timeString
    return (t, minute)


beginParser :: Parser (Maybe Int, Status)
beginParser = do
    string "Guard #"
    guardID <- L.decimal
    string " begins shift"
    return $ (Just guardID, OnGuard)


asleepParser :: Parser (Maybe Int, Status)
asleepParser = do
    string "falls asleep"
    return $ (Nothing, Asleep)


wakeParser :: Parser (Maybe Int, Status)
wakeParser = do
    string "wakes up"
    return $ (Nothing, Awake)


-- pure methods
assignIDs :: [WallEntry] -> [WallEntry]
assignIDs ws = assignAssist ws (ident . head $ ws) where
    assignAssist [] _           = []
    assignAssist (w:ws) curr
        | ident w == Nothing    = WallEntry (time w) (minute w) curr (status w) : assignAssist ws curr
        | otherwise             = w : assignAssist ws (ident w)


-- given a (chronological, with ids) list of wall entries, return id of the guard
-- who sleeps the most
sleepiestGuard :: [WallEntry] -> Maybe Int
sleepiestGuard ws = fst . last $ sortOn (\x -> snd x) sleepCounts where
    sleepCounts = zip guardIDs (map (\x -> timeAsleep $ filter (\y -> ident y == x) ws) guardIDs) where
        guardIDs = nub $ map (ident) ws


-- given the WallEntries for a single guard, figure out how much time is spent asleep
-- this assumes that all the timeAsleep we count in total is within the midnight hour
-- and assumes that everything comes in 
-- originally missing a case; its possible for guards to be diligent and not sleep!
timeAsleep :: [WallEntry] -> Int
timeAsleep [] = 0
timeAsleep [x] = 0
timeAsleep (s:w:ws)
    | status s == OnGuard   = timeAsleep (w:ws)
    | otherwise             = div (round $ diffUTCTime (time w) (time s)) 60 + timeAsleep ws


sleepiestMinute :: [WallEntry] -> (Int, Int)
sleepiestMinute ws = (fromJust $ elemIndex (maximum (wpv ws)) (wpv ws), maximum (wpv ws)) where
    wpv ws = wpvHelper (take 60 $ repeat 0) ws where
        wpvHelper acc [] = acc
        wpvHelper acc [x] = acc
        wpvHelper acc (s:w:ls)
            | status s == OnGuard   = wpvHelper acc (w:ls)
            | otherwise             = wpvHelper (zipWith (+) acc (wakePeriodVec s w)) ls

-- not actually a vec anymore ...
wakePeriodVec :: WallEntry -> WallEntry -> [Int]
wakePeriodVec s w = concat [leading, sleeping, trailing] where
    leading = take (minute s) $ repeat 0
    sleeping = take ((minute w) - (minute s)) $ repeat 1
    trailing = take (60-(minute w)) $ repeat 0



sleepiestMinutePerGuard :: [WallEntry] -> [(Int, Int, Int)]
sleepiestMinutePerGuard ws = zipWith (\x y -> (fromJust x, fst y, snd y)) guardIDs guardMinutes where
    guardIDs = nub $ map (ident) ws
    guardMinutes = map (\x -> sleepiestMinute $ filter (\y -> ident y == x) ws) guardIDs


overallSleepiestGuardMinutePair :: [WallEntry] -> (Int, Int)
overallSleepiestGuardMinutePair ws = (first (sgmp ws), second (sgmp ws)) where
    sgmp ws = last $ sortOn (\x -> third x) (sleepiestMinutePerGuard ws)

first :: (a, a, a) -> a
first (x, _, _) = x

second :: (a, a, a) -> a
second (_, x, _) = x

third :: (a, a, a) -> a
third (_, _, x) = x




-- this is a total hack and you shouldn't do things this way
extract :: Maybe Int -> Int
extract Nothing = 0
extract (Just x) = x


mainA :: IO ()
mainA = do
    ws <- parseInput "input.txt"
    let sg = sleepiestGuard ws
    let (sm, _) = sleepiestMinute $ filter (\x -> ident x == sg) ws
    let answer = (fromJust sg) * sm
    print answer
    -- result <- submitAnswer 2018 04 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    ws <- parseInput "input.txt"
    let (sg, sm) = overallSleepiestGuardMinutePair ws
    let answer = sg * sm
    result <- submitAnswer 2018 04 2 answer
    print result
    return ()
