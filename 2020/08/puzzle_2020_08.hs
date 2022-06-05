module Puzzle_2020_08 where

import Automation (submitAnswer)
import Parsing
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as S

-- data types
data OpCode = Acc | Jmp | Nop deriving (Eq, Ord, Show)
data Instruction = Instruction {op :: OpCode, arg :: Int}
                    deriving (Eq, Ord, Show)

data Console = Console 
    { program :: [Instruction]
    , iptr :: Int
    , reg :: Int
    , seen :: Set Int} deriving (Eq, Ord, Show)

-- parsing
assemblyP :: Parser [Instruction]
assemblyP = sepBy instructionP newline

instructionP :: Parser Instruction
instructionP = Instruction <$> opcodeP <* space <*> integer

opcodeP :: Parser OpCode
opcodeP = (string "acc" >> return Acc) <|>
          (string "jmp" >> return Jmp) <|>
          (string "nop" >> return Nop)  

-- functions
loopCheck :: [Instruction] -> Int
loopCheck prog = evalState looper initConsole
  where initConsole = Console prog 0 0 S.empty


looper :: MonadState Console m => m Int
looper = do
    console <- get
    if S.member (iptr console) (seen console)
        then return $ reg console
        else do
            modify $ \c -> c {seen = S.insert (iptr c) (seen c)}
            executeStep
            looper


terminateCheck :: [Instruction] -> Int
terminateCheck prog = reg . snd . head $ dropWhile (\x -> fst x == False) results
  where progs = flipOps prog
        initConsoles = map (\p -> Console p 0 0 S.empty) progs
        results = map (runState terminator) initConsoles
        -- (_, fixed) = head $ dropWhile fst $ map (runState terminator) initConsoles


terminator :: MonadState Console m => m Bool
terminator = do
    console <- get
    let halt = (iptr console) == (length $ program console)
    let revisit = S.member (iptr console) (seen console)
    case (halt, revisit) of
        (True, _)      -> return True
        (False, True)  -> return False
        (False, False) -> do
            modify $ \c -> c {seen = S.insert (iptr c) (seen c)}
            executeStep
            terminator



executeStep :: MonadState Console m => m ()
executeStep = do
    console <- get
    let instruction = program console !! iptr console
    case op instruction of
        Acc -> modify $ acc (arg instruction)
        Jmp -> modify $ jmp (arg instruction)
        Nop -> modify $ nop (arg instruction)


acc :: Int -> Console -> Console
acc val console = console { reg = (reg console) + val
                          , iptr = (iptr console) + 1}


jmp :: Int -> Console -> Console
jmp val console = console {iptr = (iptr console) + val}


nop :: Int -> Console -> Console
nop _ console = console {iptr = (iptr console) + 1}


flipOps :: [Instruction] -> [[Instruction]]
flipOps prog = helper 0 prog
  where helper :: Int -> [Instruction] -> [[Instruction]]
        helper _ [] = []
        helper n ((Instruction code val):is)
            | code == Acc = helper (n+1) is
            | otherwise   = prog' : helper (n+1) is
              where inst' = Instruction (flipOp code) val
                    prog' = (take n prog) ++ [inst'] ++ (drop (n+1) prog)



flipOp :: OpCode -> OpCode
flipOp Jmp = Nop
flipOp Nop = Jmp


-- mains

mainA :: IO ()
mainA = do
    (Just prog) <- parseInput assemblyP "08/input.txt"
    let answer = loopCheck prog
    print answer
    -- result <- submitAnswer 2020 08 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just prog) <- parseInput assemblyP "08/input.txt"
    let answer = terminateCheck prog
    print answer
    -- result <- submitAnswer 2020 08 2 answer
    -- print result
    return ()
