module Puzzle_2018_16 where

import Data.Bits ((.&.), (.|.))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector.Unboxed (Vector, (//), (!))
import qualified Data.Vector.Unboxed as V
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import System.IO

import System.IO
import Automation (submitAnswer)


-- data types
type Parser = Parsec Void String
type OPMap = IntMap OPCode


data Sample = Sample {before :: Register, after :: Register, op :: OP} deriving (Eq, Show)
data OP = OP {num :: Int, io :: OPIO} deriving (Eq, Show)
data OPIO = OPIO {a :: Int, b :: Int, c :: Int} deriving (Eq, Show)
type Register = Vector Int 
data OPCode = ADDR | ADDI 
            | MULR | MULI 
            | BANR | BANI 
            | BORR | BORI
            | SETR | SETI
            | GTIR | GTRI | GTRR
            | EQIR | EQRI | EQRR deriving (Eq, Ord, Show)


-- parsing
parseInput :: String -> IO ([Sample], [OP])
parseInput filename = do
    raw <- readFile filename
    let (Right (samples, program)) = runParser inputP "" raw
    return (samples, program)


inputP :: Parser ([Sample], [OP])
inputP = do
    samples <- samplesP
    -- count 2 (char '\n')
    program <- programP
    return (samples, program)


registerP :: Parser Register
registerP = do
    char '['
    r1 <- L.decimal
    string ", "
    r2 <- L.decimal
    string ", "
    r3 <- L.decimal
    string ", "
    r4 <- L.decimal
    char ']'
    return $ V.fromList [r1, r2, r3, r4]


samplesP :: Parser [Sample]
samplesP = many sampleP


sampleP :: Parser Sample
sampleP = do
    string "Before: "
    reg1 <- registerP
    char '\n'
    op <- opLineP
    char '\n'
    string "After:  "
    reg2 <- registerP
    -- count 2 (char '\n')
    many (char '\n')
    return $ Sample reg1 reg2 op

programP :: Parser [OP]
programP = opLineP `sepBy` char '\n'

opLineP :: Parser OP
opLineP = do
   opNum <- L.decimal
   char ' '
   a <- L.decimal
   char ' '
   b <- L.decimal
   char ' '
   c <- L.decimal
   return $ OP opNum (OPIO a b c)



-- functions
runOP :: OPCode -> OPIO -> Register -> Register
runOP code (OPIO a b c) reg = case code of
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


behavesLike :: Sample -> OPCode -> Bool
behavesLike (Sample bef aft (OP n io)) code = aft == runOP code io bef


codes :: [OPCode]
codes = [ADDR, ADDI, MULR, MULI, BANR, BANI, BORR, BORI
        , SETR, SETI, GTIR, GTRI, GTRR, EQIR, EQRI, EQRR]


possibleOPCodes :: Sample -> [OPCode]
possibleOPCodes s = filter (behavesLike s) codes


threeAmbiguous :: [Sample] -> [Sample]
threeAmbiguous ss = filter (\x -> (length $ possibleOPCodes x) >= 3) ss


opConstraints :: [Sample] -> IntMap (Set OPCode)
opConstraints ss = foldl constrainer noConst ss where
    noConst = IM.fromList $ zip [0..15] (repeat $ S.fromList codes)
    constrainer :: IntMap (Set OPCode) -> Sample -> IntMap (Set OPCode)
    constrainer im s@(Sample _ _ (OP n io)) = IM.adjust (\cs -> S.intersection cs consts) n im
        where
            consts = S.fromList $ possibleOPCodes s


opLookup :: OPMap
opLookup = IM.fromList
    [ (0, BANR)
    , (1, ADDR)
    , (2, EQRI)
    , (3, SETR)
    , (4, GTRR)
    , (5, BORI)
    , (6, GTIR)
    , (7, SETI)
    , (8, BORR)
    , (9, BANI)
    , (10, EQIR)
    , (11, EQRR)
    , (12, GTRI)
    , (13, ADDI)
    , (14, MULI)
    , (15, MULR)]

runProgram :: [OP] -> Register -> Register
runProgram [] reg = reg
runProgram ((OP n io):ops) reg = runProgram ops (runOP (opLookup IM.! n) io reg)



-- mains

mainA :: IO ()
mainA = do
    (samples, _) <- parseInput "input.txt"
    -- print $ length samples
    let answer = length $ threeAmbiguous samples
    print answer
    -- result <- submitAnswer 2018 16 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (samples, program) <- parseInput "input.txt"
    -- print $ opConstraints samples
    let finalReg = runProgram program (V.fromList [0,0,0,0])
    let answer = finalReg ! 0
    print answer
    result <- submitAnswer 2018 16 2 answer
    print result
    return ()
