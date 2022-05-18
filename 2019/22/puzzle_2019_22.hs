module Puzzle_2019_22 where

import Automation (submitAnswer)
import Parsing
import Data.List (foldl', iterate', scanl')

modulus :: Integral a => a
-- modulus = 10
modulus = 119315717514047

-- data types
type Action = Int -> Int -> Int
type Shuffle = [Action]


-- a little hacky because you need to hardcode the decksize
-- also parser could be like a one-liner or something with state monad?
-- one of the ParsecT arguments lets you put that in
data Technique a = Technique a a
    deriving (Eq, Ord, Show)


-- 1st is n -> a + b*n mod _
-- 2nd is n -> c + d*n mod m
-- composite is n -> c + d*(a + b*n)
-- order is wonky, not function application order
instance Integral a => Semigroup (Technique a) where
    (<>) (Technique a b ) (Technique c d) 
        = Technique (mod (c + d*a) modulus) (mod (d*b) modulus)


instance Integral a => Monoid (Technique a) where
    mempty = Technique 0 1


-- parsing
shuffleP :: Parser Shuffle
shuffleP = sepBy actionP newline


actionP :: Parser Action
actionP = cutP <|> dealNewStackP <|> dealIncrementP


cutP :: Parser Action
cutP = cut <$> (string "cut " >> integer)


dealNewStackP :: Parser Action
dealNewStackP = string "deal into new stack" >> return dealNewStack


dealIncrementP :: Parser Action
dealIncrementP = dealIncrement <$> (string "deal with increment " >> integer)


techniqueP :: Parser (Technique Integer)
techniqueP = mconcat <$> sepBy efficientP newline


efficientP :: Parser (Technique Integer)
efficientP = cutTechniqueP <|> stackTechniqueP <|> incrementTechniqueP


cutTechniqueP :: Parser (Technique Integer)
cutTechniqueP = (\x -> Technique (toInteger (-x)) 1)
    <$> (string "cut " >> integer)


stackTechniqueP :: Parser (Technique Integer)
stackTechniqueP = string "deal into new stack" >> (return $ Technique (-1) (-1))


incrementTechniqueP :: Parser (Technique Integer)
incrementTechniqueP = (\x -> Technique 0 (toInteger x)) 
    <$> (string "deal with increment " >> integer) 



-- functions
position :: Shuffle -> Int -> Int -> Int
position shuffle deck card = foldl' (\p s -> s deck p) card shuffle


cut :: Int -> Action
cut n deck card = mod (card - n) deck


dealNewStack :: Action
dealNewStack deck card = mod (-card - 1) deck


dealIncrement :: Int -> Action
dealIncrement n deck card = mod (n * card) deck


applyTechnique :: Integral a => Technique a -> a -> a
applyTechnique (Technique a b) card = mod (a + (b*card)) modulus


track :: Integral a => Technique a -> a -> [a]
-- track technique card = go (applyTechnique card) 1
--   where go :: Int -> Int -> Int
--         go card' n
--             | card' == card = n
--             | otherwise     = go (applyTechnique card') (n+1)
track technique card = card : takeWhile (/= card) iterates
    where iterates = tail $ scanl' (\c t -> t c) card (repeat $ applyTechnique technique) 


period :: Integral a => Technique a -> a -> a
period technique card = go (applyTechnique technique card) 1
  where go card' n
            | card' == card = n
            | otherwise     = go (applyTechnique technique card') (n+1)


-- efficient :: Technique Integer -> Integer -> Integer -> Integer
-- efficient technique loops card = cyclic !! (mod (-loops) order)
--   where cyclic = track technique card
--         order = toInteger $ length cyclic


predict :: Technique Integer -> Integer -> Integer -> Integer
predict technique loops card = go card amount
  where order = period technique card
        amount = mod (-loops) order
        go :: Integer -> Integer -> Integer
        go (!card) 0 = card
        go (!card) n = go (applyTechnique technique card) (n-1)



predict' :: Technique Integer -> Integer -> Integer -> Integer
predict' technique loops card = applyTechnique advanced card
  where power = mod (-loops) (modulus * (modulus - 1))
        advanced = exponentiate technique power


exponentiate :: (Monoid m, Integral a) => m -> a -> m
exponentiate element n = go element n mempty
  where go e k acc
            | k == 0 = acc
            | even k = go (e <> e) (div k 2) acc
            | odd  k = go (e <> e) (div k 2) (acc <> e)



-- mains

mainA :: IO ()
mainA = do
    (Just shuffle) <- parseInput shuffleP "22/input.txt"
    let answer = position shuffle 10007 2019
    print answer
    -- result <- submitAnswer 2019 22 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just technique) <- parseInput techniqueP "22/input.txt"
    print technique
    -- let answer = predict technique (101741582076661::Integer) (2020::Integer)
    let answer = predict' technique (101741582076661::Integer) (2020::Integer)
    print answer
    result <- submitAnswer 2019 22 2 answer
    print result
    return ()
