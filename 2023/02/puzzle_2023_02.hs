module Puzzle_2023_02 where

import Automation (submitAnswer)
import Parsing

-- data types
data Cubes = Cubes { red :: Int
                   , green :: Int
                   , blue :: Int} deriving (Eq, Show)

instance Semigroup Cubes where
    (Cubes a b c) <> (Cubes d e f) = Cubes (a+d) (b+e) (c+f)

instance Monoid Cubes where
    mempty = Cubes 0 0 0

instance Ord Cubes where
    (Cubes a b c) <= (Cubes d e f) = (a<=d) && (b<=e) && (c<=f)

data Game = Game { identity :: Int
                 , draws :: [Cubes]} deriving (Eq, Show)

-- parsing
gamesP :: Parser [Game]
gamesP = sepBy gameP newline

gameP :: Parser Game
gameP = do
    string "Game"
    space
    i <- natural
    char ':'
    ds <- drawsP
    return $ Game i ds

drawsP :: Parser [Cubes]
drawsP = sepBy drawP (char ';')

drawP :: Parser Cubes
drawP = mconcat <$> sepBy cubesP (char ',')

cubesP :: Parser Cubes
cubesP = do
    space
    n <- natural
    space
    c <- colourP
    return $ c n

colourP :: Parser (Int -> Cubes)
colourP = (string "red" >> return (\x -> Cubes x 0 0) )
      <|> (string "green" >> return (\x -> Cubes 0 x 0))
      <|> (string "blue" >> return (\x -> Cubes 0 0 x))

-- functions
feasible :: Cubes -> Game -> Bool
feasible bag game = all (\x -> x <= bag) (draws game)

gameSum :: Cubes -> [Game] -> Int
gameSum limit games = sum $ (map identity) (filter (feasible limit) games)

envelope :: Cubes -> Cubes -> Cubes
envelope (Cubes a b c) (Cubes d e f) = Cubes (max a d) (max b e) (max c f)

power :: Cubes -> Int
power (Cubes a b c) = a*b*c

smallestBag :: Game -> Cubes
smallestBag game = foldl (\x y -> envelope x y) mempty (draws game)

powerSum :: [Game] -> Int
powerSum games = sum $ map (power . smallestBag) games

-- mains

mainA :: IO ()
mainA = do
    (Just games) <- parseInput gamesP "02/input.txt"
    let limit = Cubes 12 13 14
    let answer = gameSum limit games
    print answer
    -- result <- submitAnswer 2023 02 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just games) <- parseInput gamesP "02/input.txt"
    let answer = powerSum games
    print answer
    -- result <- submitAnswer 2023 02 2 answer
    -- print result
    return ()
