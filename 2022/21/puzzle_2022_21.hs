module Puzzle_2022_21 where

import Automation (submitAnswer)
import Parsing
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad.Reader

-- data types
type Monkey = String
type Assignment = Map Monkey Job
data Job = Lone Int | Pair Monkey Monkey Operation


data Operation = Add
               | Sub
               | Mul
               | Div deriving (Eq, Ord, Show)


-- parsing
monkeysP :: Parser Assignment
monkeysP = M.unions <$> sepBy monkeyP newline


monkeyP :: Parser Assignment
monkeyP = do
    m <- nameP
    string ": "
    j <- jobP
    return $ M.singleton m j


nameP :: Parser Monkey
nameP = count 4 lowerChar


jobP :: Parser Job
jobP = loneP <|> pairP


loneP :: Parser Job
loneP = Lone <$> integer


pairP :: Parser Job
pairP = do
    m1 <- nameP
    char ' '
    op <- operationP
    char ' '
    m2 <- nameP
    return $ Pair m1 m2 op 


operationP :: Parser Operation
operationP = (char '+' >> return Add)
         <|> (char '-' >> return Sub)
         <|> (char '*' >> return Mul)
         <|> (char '/' >> return Div)

-- functions
evaluate :: Assignment -> Monkey -> Int
evaluate jobs monkey = runReader (yell monkey) jobs


operate :: Operation -> (Int -> Int -> Int)
operate op = case op of
                Add -> (+)
                Sub -> (-)
                Mul -> (*)
                Div -> div


yell :: MonadReader Assignment m => Monkey -> m Int
yell monkey = do
    job <- reader (M.! monkey)
    case job of
        (Lone x)        -> return x
        (Pair mA mB op) -> do
            xA <- yell mA
            xB <- yell mB
            return $ (operate op) xA xB


track :: Assignment -> Monkey -> [Monkey]
track jobs monkey = runReader (impostor monkey) jobs


impostor :: MonadReader Assignment m => Monkey -> m [Monkey]
impostor "humn" = return ["humn"]
impostor monkey = do
    job <- reader (M.! monkey)
    case job of
        (Lone x)        -> return []
        (Pair mA mB op) -> do
            tA <- impostor mA
            tB <- impostor mB
            if length (tA ++ tB) == 0
                then return []
                else return $ monkey : tA ++ tB


-- the next closest to the impostor
suspect :: MonadReader Assignment m => Monkey -> m Monkey
suspect monkey = (head . drop 1) <$> impostor monkey


-- solve the "riddle" of what "humn" needs to say to make "root" have equality
riddle :: Assignment -> Int
riddle jobs = runReader (pose) jobs


pose :: MonadReader Assignment m => m Int
pose = do
    job <- reader (M.! "root")
    case job of
        (Lone x)       -> error "Root should always be a pair"
        (Pair mA mB _) -> do
            next <- suspect "root"
            let other = if next == mA then mB else mA
            target <- yell other
            lley target next


-- it's yell but backwards, figure out 
lley :: MonadReader Assignment m => Int -> Monkey -> m Int
lley target monkey = do
    job <- reader (M.! monkey)
    case job of
        (Lone x)        -> return target
        (Pair mA mB op) -> do
            next <- suspect monkey
            target' <- do
                if next == mA
                    then solveA op target <$> yell mB
                    else solveB op target <$> yell mA
            lley target' next


-- solve for a in target = op a b
solveA :: Operation -> Int -> Int -> Int
solveA op target b = case op of
                        Add -> target - b
                        Sub -> target + b
                        Mul -> div target b
                        Div -> target * b


-- solveB for b in target = op a b
solveB :: Operation -> Int -> Int -> Int
solveB op target a = case op of
                        Add -> target - a
                        Sub -> a - target
                        Mul -> div target a
                        Div -> div a target


-- mains

mainA :: IO ()
mainA = do
    (Just monkeys) <- parseInput monkeysP "21/input.txt"
    let answer = evaluate monkeys "root"
    print answer
    -- result <- submitAnswer 2022 21 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just monkeys) <- parseInput monkeysP "21/input.txt"
    let answer = riddle monkeys
    print answer
    -- result <- submitAnswer 2022 21 2 answer
    -- print result
    return ()
