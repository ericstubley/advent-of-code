module Puzzle_2018_19 where

import Data.Bits ((.&.), (.|.))
import Data.Sequence (Seq)
import Data.Vector.Unboxed (Vector, (//), (!))
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Vector.Unboxed as V
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Sequence as S

import System.IO
import Automation (submitAnswer)


-- data types
type Parser = Parsec Void String


type Program = Seq OP
data OP = OP {_code :: OPCode, _a :: Int, _b :: Int, _c :: Int} deriving (Eq, Show)
type Register = Vector Int 
data OPCode = ADDR | ADDI 
            | MULR | MULI 
            | BANR | BANI 
            | BORR | BORI
            | SETR | SETI
            | GTIR | GTRI | GTRR
            | EQIR | EQRI | EQRR deriving (Enum, Eq, Ord, Show)


-- parsing
parseInput :: String -> IO (Int, Program)
parseInput filename = do
    raw <- readFile filename
    let (Right (ip, program)) = runParser inputP "" raw
    return (ip, S.fromList program)


inputP :: Parser (Int, [OP])
inputP = do
    ip <- instructionPointerP
    char '\n'
    -- count 2 (char '\n')
    program <- programP
    return (ip, program)


instructionPointerP :: Parser Int
instructionPointerP = do
    string "#ip "
    ip <- L.decimal
    return ip


programP :: Parser [OP]
programP = opP `sepBy` char '\n'


opcodeP :: Parser OPCode
opcodeP = do
    codeString <- choice $ (map (\s -> string s)) codeStrings 
    return $ strToCode codeString


opP :: Parser OP
opP = do
    code <- opcodeP
    char ' '
    a <- L.decimal
    char ' '
    b <- L.decimal
    char ' '
    c <- L.decimal
    return $ OP code a b c



-- functions
runOP :: OP -> Register -> Register
runOP (OP code a b c) reg = case code of
    ADDR -> reg // [(c, (reg!a) + (reg!b))]
    ADDI -> reg // [(c, (reg!a) + b)]
    MULR -> reg // [(c, (reg!a) * (reg!b))]
    MULI -> reg // [(c, (reg!a) * b)]
    BANR -> reg // [(c, (reg!a) .&. (reg!b))]
    BANI -> reg // [(c, (reg!a) .&. b)]
    BORR -> reg // [(c, (reg!a) .|. (reg!b))]
    BORI -> reg // [(c, (reg!a) .|. b)]
    SETR -> reg // [(c, reg!a)]
    SETI -> reg // [(c, a)]
    GTIR -> reg // [(c, if a        > (reg!b) then 1 else 0)]
    GTRI -> reg // [(c, if (reg!a)  > b       then 1 else 0)]
    GTRR -> reg // [(c, if (reg!a)  > (reg!b) then 1 else 0)]
    EQIR -> reg // [(c, if a       == (reg!b) then 1 else 0)]
    EQRI -> reg // [(c, if (reg!a) == b       then 1 else 0)]
    EQRR -> reg // [(c, if (reg!a) == (reg!b) then 1 else 0)]


codes :: [OPCode]
codes = [ADDR .. EQRR]


codeStrings :: [String]
codeStrings =   [ "addr", "addi", "mulr", "muli"
                , "banr", "bani", "borr", "bori"
                , "setr", "seti", "gtir", "gtri", "gtrr"
                , "eqir", "eqri", "eqrr"]


strToCode :: String -> OPCode
strToCode s = case s of
    "addr" -> ADDR
    "addi" -> ADDI
    "mulr" -> MULR
    "muli" -> MULI
    "banr" -> BANR
    "bani" -> BANI
    "borr" -> BORR
    "bori" -> BORI
    "setr" -> SETR
    "seti" -> SETI
    "gtir" -> GTIR
    "gtri" -> GTRI
    "gtrr" -> GTRR
    "eqir" -> EQIR
    "eqri" -> EQRI
    "eqrr" -> EQRR


advanceIP :: Int -> Register -> Register
advanceIP ip reg = reg // [(ip, (reg!ip) + 1)]


runProgram :: Program -> Int -> Register -> Register
runProgram program ip reg
    | pointer >= S.length program || pointer < 0  = reg
    | otherwise                         = runProgram program ip reg' where
        pointer = reg!ip
        reg' = advanceIP ip $ runOP (S.index program pointer) reg



mainA :: IO ()
mainA = do
    (ip, program) <- parseInput "input.txt"
    let reg = V.replicate 6 0
    let reg' = runProgram program ip reg
    let answer = reg' ! 0
    print answer
    -- result <- submitAnswer 2018 19 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (ip, program) <- parseInput "input.txt"
    let reg = V.fromList [1,0,0,0,0,0]
    let reg' = runProgram program ip reg
    let answer = reg' ! 0
    print answer
    result <- submitAnswer 2018 19 2 answer
    print result
    return ()
