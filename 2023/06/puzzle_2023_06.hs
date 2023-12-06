module Puzzle_2023_06 where

import Automation (submitAnswer)
import Parsing

-- data types
data Race = Race { _time :: Int 
                 , _distance :: Int } deriving (Eq, Ord, Show)

-- parsing
racesP :: Parser [Race]
racesP = do
    string "Time:" 
    times <- many (hspace >> natural)
    newline
    string "Distance:"
    distances <- many (hspace >> natural)
    return $ zipWith Race times distances

-- functions
strategies :: Race -> [Int]
strategies (Race t d) = filter (d<) . map (\n -> n*(t-n)) $ [0..t]

stratCountProd :: [Race] -> Int
stratCountProd = product . map (length . strategies)

compress :: [Race] -> Race
compress races = Race (read . concat . map (show . _time) $ races) (read . concat . map (show . _distance) $ races)

-- mains

mainA :: IO ()
mainA = do
    (Just races) <- parseInput racesP "06/input.txt"
    let answer = stratCountProd races
    print answer
    -- result <- submitAnswer 2023 06 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just races) <- parseInput racesP "06/input.txt"
    let answer = length . strategies . compress $ races
    print answer
    -- result <- submitAnswer 2023 06 2 answer
    -- print result
    return ()
