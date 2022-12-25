module Puzzle_2022_25 where

import Automation (submitAnswer)
import Parsing

-- data types
-- Mew for "Minus two", etc...
data Figit = Mew | Mun | Nun | One | Two deriving (Eq, Ord)

instance Show Figit where
    show Mew = "="
    show Mun = "-"
    show Nun = "0"
    show One = "1"
    show Two = "2"

type Fun = [Figit]

instance Num Fun where
    (+) f1 f2 = tinSum f1 f2
    fromInteger n = []
     
-- parsing
snafusP :: Parser [Fun]
snafusP = sepBy snafuP newline


snafuP :: Parser Fun
snafuP = some figitP


figitP :: Parser Figit
figitP = (char '=' >> return Mew)
     <|> (char '-' >> return Mun)
     <|> (char '0' >> return Nun)
     <|> (char '1' >> return One)
     <|> (char '2' >> return Two)


-- functions
-- tin is Sn on the periodic table, and this function is snSum
tinSum :: Fun -> Fun -> Fun
tinSum [] fs = fs
tinSum fs [] = fs
tinSum fs1 fs2
    | length fs1 < length fs2 = tinSum (pad ++ fs1) fs2
    | length fs1 > length fs2 = tinSum fs1 (pad ++ fs2)
    | otherwise               = dropWhile (==Nun) $ tinHelper Nun fs1 fs2
      where pad = take (abs (length fs1 - length fs2)) $ repeat Nun


tinHelper :: Figit -> Fun -> Fun -> Fun
tinHelper car [] [] = [car]
tinHelper car fs1 fs2 = tinHelper (carry x) (init fs1) (init fs2) ++ [remain x]
  where x = (fint car) + (fint $ last fs1) + (fint $ last fs2)


carry :: Int -> Figit
carry x
    | -5 <= x && x <= -3 = Mun
    | -2 <= x && x <=  2 = Nun
    |  3 <= x && x <=  5 = One
    | otherwise          = error "Unexpected digit sum"


remain :: Int -> Figit
remain x
    | x == -5   = Nun
    | x == -4   = One
    | x == -3   = Two
    | x == -2   = Mew
    | x == -1   = Mun
    | x ==  0   = Nun
    | x ==  1   = One
    | x ==  2   = Two
    | x ==  3   = Mew
    | x ==  4   = Mun
    | x ==  5   = Nun
    | otherwise = error "Unexpected digit sum"


fint :: Figit -> Int
fint f = case f of
            Mew -> -2
            Mun -> -1
            Nun ->  0
            One ->  1
            Two ->  2


printer :: Fun -> String
printer = concat . map show

-- mains

mainA :: IO ()
mainA = do
    (Just snafus) <- parseInput snafusP "25/input.txt"
    let answer = printer $ sum snafus
    print answer
    -- result <- submitAnswer 2022 25 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    let answer = 0
    print answer
    -- result <- submitAnswer 2022 25 2 answer
    -- print result
    return ()
