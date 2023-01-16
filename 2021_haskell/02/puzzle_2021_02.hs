module Puzzle_2021_02 where

import Automation (submitAnswer)
import Parsing
import Control.Monad.State

-- data types
data Command = Forward Int | Down Int | Up Int deriving (Eq, Ord, Show)

type Aim = Int

-- parsing
courseP :: Parser [Command]
courseP = sepBy commandP newline


commandP :: Parser Command
commandP = (Forward <$> lineP "forward")
       <|> (Down <$> lineP "down")
       <|> (Up <$> lineP "up")


lineP :: String -> Parser Int
lineP s = string s >> space >> integer


-- functions
navigate :: [Command] -> (Int, Int)
navigate course = foldl updater (0, 0) course


updater :: (Int, Int) -> Command -> (Int, Int)
updater (h, d) (Forward x) = (h + x, d)
updater (h, d) (Down x)    = (h, d + x)
updater (h, d) (Up x)      = (h, d - x)


navigate' :: [Command] -> (Int, Int)
navigate' course = evalState (foldM navigator (0, 0) course) 0


navigator :: MonadState Aim m => (Int, Int) -> Command -> m (Int, Int)
navigator (h, d) (Down x)    = modify (\a -> a + x) >> return (h, d)
navigator (h, d) (Up x)      = modify (\a -> a - x) >> return (h, d)
navigator (h, d) (Forward x) = gets (\a -> (h + x, d + a*x))


extract :: (Int, Int) -> Int
extract (h, d) = h * d


-- mains

mainA :: IO ()
mainA = do
    (Just course) <- parseInput courseP "02/input.txt"
    let answer = extract $ navigate course
    print answer
    -- result <- submitAnswer 2021 02 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just course) <- parseInput courseP "02/input.txt"
    let answer = extract $ navigate' course
    print answer
    -- result <- submitAnswer 2021 02 2 answer
    -- print result
    return ()
