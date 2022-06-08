module Puzzle_2020_14 where

import Automation (submitAnswer)
import Parsing
import Data.Bits
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

-- data types
data Operation = Mask (Int -> Int) | Mem Int Int
data OperationV2 = MaskV2 ([Int] -> [Int]) | MemV2 Int Int

-- parsing
operationsP :: Parser [Operation]
operationsP = sepBy operationP newline

operationP :: Parser Operation
operationP = maskP <|> memP

maskP :: Parser Operation
maskP = do
    string "mask = "
    bits <- count 36 bitP
    return $ Mask (constructMask bits)


bitP :: Parser Char
bitP = char 'X' <|> char '1' <|> char '0'


memP :: Parser Operation
memP = do
    string "mem["
    address <- integer
    string "] = "
    value <- integer
    return $ Mem address value

operationsV2P :: Parser [OperationV2]
operationsV2P = sepBy operationV2P newline

operationV2P :: Parser OperationV2
operationV2P = maskV2P <|> memV2P

maskV2P :: Parser OperationV2
maskV2P = do
    string "mask = "
    bits <- count 36 bitP
    return $ MaskV2 (constructMaskV2 bits)

memV2P :: Parser OperationV2
memV2P = do
    string "mem["
    address <- integer
    string "] = "
    value <- integer
    return $ MemV2 address value

-- functions
constructMask :: [Char] -> (Int -> Int)
constructMask bits = builder bits 35
  where builder :: [Char] -> Int -> (Int -> Int)
        builder [] _ = id
        builder (b:bs) n =
            case b of
                'X' -> builder bs (n-1)
                '1' -> (\x -> setBit x n)   . (builder bs (n-1))
                '0' -> (\x -> clearBit x n) . (builder bs (n-1))


runProgram :: [Operation] -> Map Int Int
runProgram operations = go id M.empty operations
  where go :: (Int -> Int) -> Map Int Int -> [Operation] -> Map Int Int
        go _ mem [] = mem
        go mask mem (o:os) = 
            case o of
                (Mask f) -> go f mem os
                (Mem address value) -> go mask mem' os
                  where mem' = M.insert address (mask value) mem


constructMaskV2 :: [Char] -> ([Int] -> [Int])
constructMaskV2 bits = builder bits 35
  where builder :: [Char] -> Int -> ([Int] -> [Int])
        builder [] _ = id
        builder (b:bs) n = (fancyMask b n) . (builder bs (n-1))


fancyMask :: Char -> Int -> ([Int] -> [Int])
fancyMask c n = case c of
                'X' -> \xs -> (map (\x -> setBit x n) xs) ++ (map (\x -> clearBit x n) xs)
                '1' -> \xs -> map (\x -> setBit x n) xs
                '0' -> id


runProgramV2 :: [OperationV2] -> Map Int Int
runProgramV2 operations = go id M.empty operations
  where go :: ([Int] -> [Int]) -> Map Int Int -> [OperationV2] -> Map Int Int
        go _ mem [] = mem
        go mask mem (o:os) = 
            case o of
                (MaskV2 f) -> go f mem os
                (MemV2 address value) -> go mask mem' os
                  where newVals = M.fromList $ zip (mask [address]) (repeat value)
                        mem' = M.union newVals mem


valueSum :: Map Int Int -> Int
valueSum mem = sum $ M.elems mem



-- mains

mainA :: IO ()
mainA = do
    (Just operations) <- parseInput operationsP "14/input.txt"
    let answer = valueSum . runProgram $ operations
    print answer
    -- result <- submitAnswer 2020 14 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just operations) <- parseInput operationsV2P "14/input.txt"
    let answer = valueSum . runProgramV2 $ operations
    print answer
    -- result <- submitAnswer 2020 14 2 answer
    -- print result
    return ()
