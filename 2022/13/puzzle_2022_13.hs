module Puzzle_2022_13 where

import Automation (submitAnswer)
import Parsing
import Data.Sort (sort)
import Data.Maybe (fromJust)
import Data.List (elemIndex)

-- data types
data Packet = Value Int | List [Packet] deriving (Eq)

instance Ord Packet where
    -- two values
    compare (Value l) (Value r) = compare l r
    -- one values
    compare (Value l) r = compare (List [Value l]) r
    compare l (Value r) = compare l (List [Value r])
    -- empty lists
    compare (List []) (List []) = EQ
    -- one empty list
    compare (List []) _ = LT
    compare _ (List []) = GT
    -- two non-empty lists
    compare (List (l:ls)) (List (r:rs)) = case compare l r of
                                            LT -> LT
                                            GT -> GT
                                            EQ -> compare ls rs

-- parsing
packetsP :: Parser [Packet]
packetsP = sepBy packetP (count' 1 2 newline)


packetPairsP :: Parser [(Packet, Packet)]
packetPairsP = sepBy packetPairP (count 2 newline)


packetPairP :: Parser (Packet, Packet)
packetPairP = do
    p1 <- packetP
    newline
    p2 <- packetP
    return (p1, p2)


packetP :: Parser Packet
packetP = valueP <|> listP


valueP :: Parser Packet
valueP = Value <$> natural


listP :: Parser Packet
listP = List <$> (between (char '[') (char ']') (sepBy packetP (char ',')))


-- functions
comparisons :: [(Packet, Packet)] -> [Ordering]
comparisons = map (\t -> compare (fst t) (snd t))


correctIndices :: [(Packet, Packet)] -> [Int]
correctIndices pps = [i | (cp, i) <- zip (comparisons pps) [1..], cp == LT]


dividerPacketA :: Packet
dividerPacketA = List [List [Value 2]]


dividerPacketB :: Packet
dividerPacketB = List [List [Value 6]]


counter :: Eq a => a -> [a] -> Int
counter x = length . filter (x==)


decoderKey :: [Packet] -> Int
decoderKey packets = indexA * indexB where
    packets' = dividerPacketA : dividerPacketB : packets
    indexA = counter True $ map (<= dividerPacketA) packets'
    indexB = counter True $ map (<= dividerPacketB) packets'


-- mains

mainA :: IO ()
mainA = do
    (Just packetPairs) <- parseInput packetPairsP "13/input.txt"
    let answer = sum . correctIndices $ packetPairs
    print answer
    -- result <- submitAnswer 2022 13 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just packets) <- parseInput packetsP "13/input.txt"
    let answer = decoderKey packets
    print answer
    -- result <- submitAnswer 2022 13 2 answer
    -- print result
    return ()
