module Puzzle_2022_11 where

import Automation (submitAnswer)
import Parsing
import Data.Sort (sort)

-- data types
data Monkey = Monkey
    { _index :: Int
    , _update :: Int -> Int
    , _test :: Int -> Bool
    , _success :: Int
    , _failure :: Int}


data Item = Item
    { _holder :: Int
    , _worry :: Int} deriving (Eq, Ord, Show)

-- parsing
monkeysP :: Parser ([Monkey], [Item])
monkeysP = do
    xs <- sepBy monkeyP newline
    let ms = map fst xs
    let is = concat . map snd $ xs
    return (ms, is)


monkeyP :: Parser (Monkey, [Item])
monkeyP = do
    string "Monkey "
    index <- natural
    string ":" 
    newline
    items <- itemsP index
    newline
    update <- updateP
    newline
    test <- testP
    newline
    success <- successP
    newline
    failure <- failureP
    optional newline
    return $ (Monkey index update test success failure, items)


itemsP :: Int -> Parser [Item]
itemsP holder = string "  Starting items: " >> many (itemP holder)


itemP :: Int -> Parser Item
itemP holder = do
    n <- natural
    optional (string ", ")
    return $ Item holder n


updateP :: Parser (Int -> Int)
updateP = string "  Operation: new = old " >> (squareP <|> multP <|> addP)


squareP :: Parser (Int -> Int)
squareP = do
    string "* old"
    return $ \x -> x*x


multP :: Parser (Int -> Int)
multP = do
    string "* "
    n <- natural
    return $ \x -> x*n


addP :: Parser (Int -> Int)
addP = do
    string "+ "
    n <- natural
    return $ \x -> x+n


testP :: Parser (Int -> Bool)
testP = do
    string "  Test: divisible by "
    n <- natural
    return (\x -> mod x n == 0)


successP :: Parser Int
successP = string "    If true: throw to monkey " >> natural


failureP :: Parser Int
failureP = string "    If false: throw to monkey " >> natural


-- functions
inspect :: Monkey -> Item -> Item
inspect m i
    | _index m == _holder i = Item holder' worry'
    | otherwise = i
      where worry' = update m i
            holder' = case _test m worry' of
                        True -> _success m
                        False -> _failure m


update :: Monkey -> Item -> Int
update m i = div (_update m (_worry i)) 3


keepAwayRound :: [Monkey] -> [Item] -> [Item]
keepAwayRound monkeys items = foldl (\is m -> act m is) items monkeys


heldRound :: [Monkey] -> [Item] -> [Int]
heldRound [] _ = []
heldRound (m:ms) items = countHeld m items : heldRound ms (act m items)


countHeld :: Monkey -> [Item] -> Int
countHeld m is = length . filter (\i -> _index m == _holder i) $ is


act :: Monkey -> [Item] -> [Item]
act m is = map (inspect m) is



totalHeld :: Int -> [Monkey] -> [Item] -> [Int]
totalHeld 0 monkeys items = take (length monkeys) $ repeat 0 
totalHeld n monkeys items = zipWith (+) held recursed
  where held = heldRound monkeys items
        recursed = totalHeld (n-1) monkeys (keepAwayRound monkeys items)


topTwo :: [Monkey] -> [Item] -> [Int]
topTwo monkeys items = take 2 . reverse . sort $ totalHeld 20 monkeys items


-- new architecture for part b

-- 96577 for testing
-- 9699690 for actual puzzle 
inspect' :: Monkey -> Item -> Item
inspect' m i = Item holder' worry'
  where worry' = mod (_update m (_worry i)) 9699690
        holder' = case _test m worry' of
                    True -> _success m
                    False -> _failure m


-- track for n rounds the given item
track :: Int -> [Monkey] -> Item -> [Int]
track n monkeys item = track' checks item
  where checks = take (n* length monkeys) (cycle monkeys)


track' :: [Monkey] -> Item -> [Int]
track' [] _ = []
track' (m:ms) i
    | _index m == _holder i = _index m : track' ms (inspect' m i)
    | otherwise             = track' ms i


counter :: Eq a => a -> [a] -> Int
counter x = length . filter (==x)


longRun :: Int -> [Monkey] -> [Item] -> [Int]
longRun n monkeys items = map (\x -> counter x longTrack) [0..(lm-1)]
  where lm = length monkeys
        longTrack = concat . map (track n monkeys) $ items


longTopTwo :: [Monkey] -> [Item] -> [Int]
longTopTwo monkeys items = take 2 . reverse . sort $ longRun 10000 monkeys items



-- mains

mainA :: IO ()
mainA = do
    (Just (monkeys, items)) <- parseInput monkeysP "11/input.txt"
    let answer = product $ topTwo monkeys items
    print answer
    -- result <- submitAnswer 2022 11 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    print "Make sure you remembered to change the modulus in inspect'"
    (Just (monkeys, items)) <- parseInput monkeysP "11/input.txt"
    let answer = product $ longTopTwo monkeys items
    print answer
    -- result <- submitAnswer 2022 11 2 answer
    -- print result
    return ()
