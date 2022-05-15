module Intcode ( Program
               , programP
               , programFromString
               , runProgram
               , runProgramAscii
               , runInteractive
               , runInteractiveAscii
               , intcodePipe
               , intcodePipeAscii) where


import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Data.Char (ord, chr)
import Data.Conduino
import Data.Maybe
import Data.Void (Void)
import Parsing (Parser, runParser, sepBy, integer, char)
import Lens.Micro.Platform
import Data.IntMap.Strict (IntMap)
import qualified Data.Conduino.Combinators as C
import qualified Data.Conduino.Lift as L
import qualified Data.IntMap.Strict as M


-- everything that we export that deals with intcodes programs should just 
-- treat them as lists, even though the internals may be very different
type Program = [Int]

-- use this type for memory locations in intcode programs
-- just a synonym o help with conceptual clarity when reading type signatures
type Index = Int

-- abstract monadic interface for an Intcode VM
-- basically it's a glorified state monad
-- is this an idomatic way to do things? this feels like it's not really
-- something that general and you're using it to emulate oop...
-- also the fact that there's basically only one instance seems a bit silly

class Monad m => VMInternalInterface m where
    vmCurr   :: m Index
    vmRead   :: m Int
    vmSeek   :: Index -> m ()
    vmPeek   :: Index -> m Int
    vmWrite  :: Index -> Int -> m()
    vmBase   :: m Index
    vmOffset :: Index -> m ()


class VMInternalInterface m => VMInterface m where
    vmInput  :: m (Maybe Int)
    vmOutput :: Int -> m ()


-- immutable intmap memory and StateT VMInternalInterface

data MapVM = MapVM
    { iptr :: Index
    , base :: Index
    , prog :: IntMap Int}
    deriving (Eq, Show)


instance Monad m => VMInternalInterface (StateT MapVM m) where
    vmCurr = gets iptr
    vmRead = do
        vm <- get
        let pos = iptr vm
        put $ vm {iptr = pos + 1}
        return $ M.findWithDefault 0 pos (prog vm)
    vmSeek pos = do
        if pos < 0
            then error $ "Tried to seek to negative memory " ++ show pos
            else modify (\vm -> vm {iptr = pos})
    vmPeek pos = gets (\vm -> M.findWithDefault 0 pos (prog vm))
    vmWrite pos val = do
        vm <- get
        let p' = M.insert pos val (prog vm)
        put $ vm {prog = p'}
    vmBase = gets base
    vmOffset val = do
        vm <- get
        let b = base vm
        put $ vm {base = val + b}


-- you can make a pipe out of the internal interface, and then use the pipe
-- for the external parts of the interface

instance VMInternalInterface m => VMInternalInterface (Pipe i o u m) where
    vmCurr = lift vmCurr
    vmRead = lift vmRead
    vmSeek pos = lift $ vmSeek pos
    vmPeek pos = lift $ vmPeek pos
    vmWrite pos val = lift $ vmWrite pos val
    vmBase = lift vmBase
    vmOffset val = lift $ vmOffset val


instance VMInternalInterface m => VMInterface (Pipe Int Int u m) where
    vmInput = await
    vmOutput val = yield val


-- basic data enums for Ops and Modes and Exiting

data Op = Add -- addition
        | Mul -- multiplication
        | Ipt -- input
        | Opt -- output
        | Jnz -- jump if not zero
        | Jez -- jump if equal zero
        | Clt -- compare less than
        | Ceq -- compare equal
        | Rbo -- relative base offset
        | Hlt -- halt
        deriving (Eq, Ord, Show)

data Mode = Pos -- positional
          | Imm -- immediate
          | Rel -- relative
          deriving (Eq, Ord, Show)

data ExitCode = Continue
              | Output
              | Halt deriving (Eq, Ord, Show)


-- how to run an intcode program:
-- step does one "clock cycle" of the vm
-- this clock cycle sort of happens in 3 parts
--      1: selectOp figure out what op we're going to do and with what modes
--      2: performOp does the actual logic, often farming it out to some
--          helper functions (for i.e. binary operations)
--      3: send out an ExitCode to say whether to continue or not

step :: (VMInterface m) => m ExitCode
step = selectOp >>= performOp


selectOp :: (VMInterface m) => m (Op, [Mode])
selectOp = do
    reg <- vmRead
    let (mn, opNum) = divMod reg 100
    let op = case opNum of
                1  -> Add
                2  -> Mul
                3  -> Ipt
                4  -> Opt
                5  -> Jnz
                6  -> Jez
                7  -> Clt
                8  -> Ceq
                9  -> Rbo
                99 -> Hlt
                _  -> error $ "Unknown opcode " ++ show opNum
    let ms = modes mn
    return (op, ms)


performOp :: (VMInterface m) => (Op, [Mode]) -> m ExitCode 
performOp (op, ms) = case op of
    Add -> binaryOperation ms (+) >> return Continue
    Mul -> binaryOperation ms (*) >> return Continue
    Ipt -> do
        i <- target (head ms)
        inp <- vmInput
        case inp of
            Nothing -> error "Ran out of input"
            (Just val) -> vmWrite i val
        return Continue
    Opt -> do
        x <- value (head ms)
        vmOutput x
        return Output
    Jnz -> jump ms (/= 0) >> return Continue
    Jez -> jump ms (== 0) >> return Continue
    Clt -> do 
        binaryOperation ms (\x y -> if x < y then 1 else 0)
        return Continue
    Ceq -> do
        binaryOperation ms (\x y -> if x == y then 1 else 0)
        return Continue
    Rbo -> do
        b <- value (head ms)
        vmOffset b
        return Continue
    Hlt -> return Halt


-- utility functions for the instructions

-- infinite list of modes extracted from a number; we only ever access up to
-- 3 elements from this list
modes :: Int -> [Mode]
modes mn = m : modes (div mn 10) where
    m = case (mod mn 10) of
        0 -> Pos
        1 -> Imm
        2 -> Rel
        _ -> error $ "Unknown mode " ++ show (mod mn 10)


value :: (VMInterface m) => Mode -> m Int
value m = case m of
    Pos -> vmRead >>= vmPeek
    Imm -> vmRead
    Rel -> do
        i <- vmRead
        b <- vmBase
        vmPeek (i+b)


-- use the mode to find a target index
-- the logic for what index to target is slightly different than when
-- extracting a value for some operation
target :: (VMInterface m) => Mode -> m Index
target m = case m of
    Pos -> vmRead
    Rel -> do
        i <- vmRead
        b <- vmBase
        return (i+b)


binaryOperation :: (VMInterface m) => [Mode] -> (Int -> Int -> Int) -> m ()
binaryOperation ms f = do
    x <- value  (ms !! 0)
    y <- value  (ms !! 1)
    i <- target (ms !! 2)
    vmWrite i (f x y)


jump :: (VMInterface m) => [Mode] -> (Int -> Bool) -> m ()
jump ms f = do
    x <- value (ms !! 0)
    y <- value (ms !! 1)
    if f x then vmSeek y
           else return ()


-- control flow of vm
-- we'll export both regular and ascii versions of the things
-- the basic things to support are
--      a. programmatic mode: pass in an input list, return an output list
--      b. interative mode: what it says on the box
--      c. pipe mode: just provide a pipe that can be hooked up to whatever
--          the problem demands

execute :: (VMInterface m) => m ()
execute = do
    exitCode <- step 
    case exitCode of
        Continue -> execute
        Output   -> execute
        Halt     -> return ()


runProgram :: Program -> [Int] -> (Program, [Int])
runProgram program input = runPipePure
     $ C.sourceList input
    .| intcodePipe program
    &| C.sinkList


runProgramAscii :: Program -> String -> (Program, String)
runProgramAscii program input = runPipePure 
     $ C.sourceList input
    .| intcodePipeAscii program
    &| C.sinkList


runInteractive :: Program -> IO ()
runInteractive program = runPipe
    $  C.stdinLines
    .| C.map (read :: String -> Int)
    .| intcodePipe program
    .| C.mapM (print :: Int -> IO ())
    .| C.sinkNull


runInteractiveAscii :: Program -> IO ()
runInteractiveAscii program = runPipe
    $  C.stdinLines
    .| C.map (read :: String -> Int)
    .| intcodePipe program
    .| C.mapM (print :: Int -> IO ())
    .| C.sinkNull


intcodePipe :: Monad m => Program -> Pipe Int Int u m Program
intcodePipe program = fmap extractor $ L.execStateP vm execute
    where vm = initVM program
          extractor vm' = mapToProgram $ prog vm'


intcodePipeAscii :: Monad m => Program -> Pipe Char Char u m Program
intcodePipeAscii program = C.map ord .| intcodePipe program .| C.map chr 


-- general utility functions

programP :: Parser Program
programP = sepBy integer (char ',')


programFromString :: String -> Maybe Program
programFromString s = do
    let result = runParser programP "" s
    case result of
        (Left err) -> Nothing
        (Right r)  -> Just r


programToMap :: Program -> IntMap Int
programToMap p = M.fromList (zip [0..] p)


-- convert a map to a program (list of ints)
-- slightly involved because the map may have "gaps" in the memory that we
-- need to fill in
mapToProgram :: IntMap Int -> Program
mapToProgram m = M.elems $ M.union m blankProgram
    where blankProgram = M.fromList $ zip [0..eom] (repeat 0)
          eom = fst $ M.findMax m


initVM :: Program -> MapVM
initVM program = MapVM 0 0 (programToMap program)
