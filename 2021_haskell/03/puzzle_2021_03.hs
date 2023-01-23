module Puzzle_2021_03 where

import Automation (submitAnswer)
import Parsing
import Data.Bits
import Data.Vector (Vector)
import qualified Data.Vector as V

-- data types
type BitString = Vector Int

-- parsing
reportP :: Parser [BitString]
reportP = sepBy bitStringP newline


bitStringP :: Parser BitString
bitStringP = V.fromList <$> many bitP


bitP :: Parser Int
bitP = (char '1' >> return 1) 
   <|> (char '0' >> return 0)


-- functions
interpret :: BitString -> Int
interpret bs = V.foldl (\acc b -> if b == 0 then 2*acc else (2*acc + 1)) 0 bs


gammaRate :: [BitString] -> BitString
gammaRate xs = V.map (\x -> div (2*x) l) $ foldl1 (V.zipWith (+)) xs
  where l = length xs


epsilonRate :: [BitString] -> BitString
epsilonRate xs = V.map (1 - ) (gammaRate xs)


powerConsumption :: [BitString] -> Int
powerConsumption xs = (interpret $ gammaRate xs) * (interpret $ epsilonRate xs)


-- part b
rating :: ([BitString] -> BitString) -> [BitString] -> BitString
rating f xs = go xs 0 where
    go :: [BitString] -> Int -> BitString
    go [bs] _ = bs
    go bss  n = go bss' (n+1) where
        bss' = filter (\b -> b V.! n == cond V.! n) bss
        cond = f bss


oxyRating :: [BitString] -> BitString
oxyRating = rating gammaRate


co2Rating :: [BitString] -> BitString
co2Rating = rating epsilonRate


lifeSupportRating :: [BitString] -> Int
lifeSupportRating xs = (interpret $ oxyRating xs) * (interpret $ co2Rating xs)



-- mains

mainA :: IO ()
mainA = do
    (Just report) <- parseInput reportP "03/input.txt"
    let answer = powerConsumption report
    print answer
    -- result <- submitAnswer 2021 03 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just report) <- parseInput reportP "03/input.txt"
    let answer = lifeSupportRating report
    print answer
    -- result <- submitAnswer 2021 03 2 answer
    -- print result
    return ()
