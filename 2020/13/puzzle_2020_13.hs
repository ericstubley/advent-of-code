module Puzzle_2020_13 where

import Automation (submitAnswer)
import Parsing
import Utilities (minimumWith)

-- parsing

scheduleP :: Parser (Int, [Int])
scheduleP = do
    time <- integer
    newline
    routes <- sepBy routeP (char ',')
    return (time, filter (/= (-1)) routes)

routeP :: Parser Int
routeP = (char 'x' >> return (-1)) <|> integer


contestP :: Parser [(Int, Int)]
contestP = do
    routes <- sepBy routeP (char ',')
    let pairs = zip routes ([0..])
    return $ filter (\x -> fst x /= (-1)) pairs


testConP :: Parser [[(Int, Int)]]
testConP = sepBy contestP newline

actualConP :: Parser [(Int, Int)]
actualConP = integer >> newline >> contestP


-- functions

waitingTime :: Int -> Int -> Int
waitingTime time bus = mod (-time) bus


earliestBus :: Int -> [Int] -> Int
earliestBus time buses = minimumWith (waitingTime time) buses


solutionA :: Int -> [Int] -> Int
solutionA time buses = eb * wt
  where eb = earliestBus time buses
        wt = waitingTime time eb


winner :: [(Int, Int)] -> Int
winner contest = soln 
  where reformat (n, a) = (n, mod (-a) n)
        contest' = map reformat contest
        (_, soln) = foldl1 crt contest'



crt :: (Int, Int) -> (Int, Int) -> (Int, Int)
crt (m, a) (n, b) = (lcm m n, soln)
  where opts = [a + k*m | k <- [0..]]
        soln = head $ dropWhile (\x -> mod x n /= b) opts


verify :: [(Int, Int)] -> Int -> Bool
verify contest soln = all checker contest
  where checker (m, a) = mod soln m == mod (-a) m


-- mains

mainA :: IO ()
mainA = do
    (Just (time, buses)) <- parseInput scheduleP "13/input.txt"
    let answer = solutionA time buses
    print answer
    -- result <- submitAnswer 2020 13 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just contest) <- parseInput actualConP "13/input.txt"
    let answer = winner contest
    print $ verify contest answer
    print answer
    -- result <- submitAnswer 2020 13 2 answer
    -- print result
    return ()
