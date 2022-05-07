module Intcode ( Program
               , MapVM (..)
               , iptr
               , prog
               , ExitCode (..)
               , programP
               , programFromString
               -- , prettyFormat
               -- , prettyPrint
               , execute
               , runProgram
               , runOutput) where


import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Data.Maybe
import Parsing (Parser, runParser, sepBy, integer, char)
import Lens.Micro.Platform
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
-- import Data.Vector.Unboxed (Vector)
-- import qualified Data.Vector.Unboxed as V


-- abstract monadic interface for an Intcode VM
-- along with instances
-- currently supporting:
--      pure RWS monad vector
--      pure RWS monad intmap


class Monad m => VMInterface m where
    vmCurr   :: m Index
    vmRead   :: m Int
    vmSeek   :: Index -> m ()
    vmPeek   :: Index -> m Int        -- given an index, peek
    vmWrite  :: Index -> Int -> m()   -- given an index and a value
    vmInput  :: m Int
    vmOutput :: Int -> m ()
    vmBase   :: m Index
    vmOffset :: Index -> m ()


-- use this type for memory locations in intcode programs
-- just to help with conceptual clarity when i.e. reading type signatures
type Index = Int

-- everything that we export that deals with program should just treat them 
-- as lists, even though the internals may be very different
type Program = [Int]

-- immutable vector memory and RWS Interface
-- RWS used for Input (Reader), Output (Writer), VectorVM (State)
-- the inputPtr is a bit of a hack right now, just so you can get
--      something running without pipes

-- commenting this out to avoid lens name conflicts, but it would be nice to
-- reenable support for this eventually
-- data VectorVM = VectorVM 
--     { _iptr :: Index
--     , _prog :: Vector Int
--     , _inputPtr :: Int} deriving (Eq, Show)
-- makeLenses ''VectorVM
-- 
-- 
-- instance Monad m => VMInterface (RWST [Int] [Int] VectorVM m) where
--     vmCurr = do
--         vm <- get
--         return $ vm ^. iptr
--     vmRead = do
--         vm <- get
--         let pos = vm ^. iptr
--         put $ vm & iptr .~ (pos + 1)
--         return $ (vm ^. prog) V.! pos
--     vmSeek pos = do
--         vm <- get
--         put $ vm & iptr .~ pos 
--     vmPeek pos = do
--         vm <- get
--         return $ (vm ^. prog) V.! pos
--     vmWrite pos val = do
--         vm <- get
--         let p' = (vm ^. prog) V.// [(pos, val)]
--         put $ vm & prog .~ p'
--     vmInput = do
--         vm <- get
--         let pos = vm ^. inputPtr
--         put $ vm & inputPtr .~ (pos + 1)
--         asks $ ( !! pos)
--     vmOutput val = tell [val]


-- immutable intmap memory and RWS interface
-- same strategy as when we did it for vectors
-- still don't have a fix for the inputPtr hack

data MapVM = MapVM
    { _iptr :: Index
    , _prog :: IntMap Int
    , _base :: Index
    , _inputPos :: Int}
    deriving (Eq, Show)

makeLenses ''MapVM


instance Monad m => VMInterface (RWST [Int] [Int] MapVM m) where
    vmCurr = do
        vm <- get
        return $ vm ^. iptr
    vmRead = do
        vm <- get
        let pos = vm ^. iptr
        put $ vm & iptr .~ (pos + 1)
        return $ M.findWithDefault 0 pos (vm ^. prog)
    vmSeek pos = do
        vm <- get
        put $ vm & iptr .~ pos
    vmPeek pos = do
        vm <- get
        return $ M.findWithDefault 0 pos (vm ^. prog)
    vmWrite pos val = do
        vm <- get
        let p' = M.insert pos val (vm ^. prog)
        put $ vm & prog .~ p'
    vmInput = do
        vm <- get
        let pos = vm ^. inputPos
        put $ vm & inputPos .~ (pos + 1)
        asks $ ( !! pos)
    vmOutput val = tell [val]
    vmBase = do
        vm <- get
        return $ vm ^. base
    vmOffset val = do
        vm <- get
        let b = vm ^. base
        put $ vm & base .~ (val + b)


-- basic data enums for Ops and Modes

data OpName = Add -- addition
            | Mul -- multiplication
            | Get -- input
            | Put -- output
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


-- what's the monadic pipeline for running programs?
-- a single step is
-- extract the current reg via a read
--      if we're incrementing the iptr this is our only chance to read modes etc.
-- split into modes and instruction with divMod
-- all the op functions take in a mode block
-- perform the op
-- do some post-processing: the common write pattern, jumping, halting

step :: (VMInterface m) => m ExitCode
step = selectOp >>= performOp


selectOp :: (VMInterface m) => m (OpName, [Mode])
selectOp = do
    reg <- vmRead
    let (mn, opNum) = divMod reg 100
    let op = case opNum of
                1  -> Add
                2  -> Mul
                3  -> Get
                4  -> Put
                5  -> Jnz
                6  -> Jez
                7  -> Clt
                8  -> Ceq
                9  -> Rbo
                99 -> Hlt
                _  -> error $ "Unknown opcode " ++ show opNum
    let ms = modes mn
    return (op, ms)


performOp :: (VMInterface m) => (OpName, [Mode]) -> m ExitCode 
performOp (op, ms) = case op of
    Add -> do
        (x, y, i) <- vvt ms 
        vmWrite i (x+y)
        return Continue
    Mul -> do
        (x, y, i) <- vvt ms 
        vmWrite i (x*y)
        return Continue
    Get -> do
        i <- target (head ms)
        inp <- vmInput
        vmWrite i inp
        return Continue
    Put -> do
        x <- value (head ms)
        vmOutput x
        return Output
    Jnz -> do
        (x, y) <- vv ms
        when (x /= 0) $ vmSeek y
        return Continue
    Jez -> do
        (x, y) <- vv ms
        when (x == 0) $ vmSeek y
        return Continue
    Clt -> do
        (x, y, i) <- vvt ms
        if x < y
            then vmWrite i 1
            else vmWrite i 0
        return Continue
    Ceq -> do
        (x, y, i) <- vvt ms
        if x == y
            then vmWrite i 1
            else vmWrite i 0
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


target :: (VMInterface m) => Mode -> m Index
target m = case m of
    Pos -> vmRead
    Rel -> do
        i <- vmRead
        b <- vmBase
        return (i+b)

vt :: (VMInterface m) => [Mode] -> m (Int, Index)
vt ms = do
    x <- value (ms !! 0)
    i <- target (ms !! 1)
    return (x, i)


vv :: (VMInterface m) => [Mode] -> m (Int, Int)
vv ms = do
    x <- value (ms !! 0)
    y <- value (ms !! 1)
    return (x, y)


vvt :: (VMInterface m) => [Mode] -> m (Int, Int, Index)
vvt ms = do
    x <- value (ms !! 0)
    y <- value (ms !! 1)
    i <- target (ms !! 2)
    return (x, y, i)


-- control flow of vm
-- need an execute
-- execute in interactive mode would be a nice option?

execute :: (VMInterface m) => m ()
execute = do
    exitCode <- step 
    case exitCode of
        Continue -> execute
        Output   -> execute
        Halt     -> return ()


executeToOutput :: (VMInterface m) => m ExitCode
executeToOutput = do
    exitCode <- step
    case exitCode of
        Continue -> executeToOutput
        _        -> return exitCode


runProgram :: Program -> [Int] -> (Program, [Int])
runProgram program inputs = (program', outputs) where
    vm = MapVM 0 (M.fromList (zip [0..] program)) 0 0
    (vm', outputs) = execRWS execute inputs vm
    program' = M.elems $ M.union (vm' ^. prog) blankProgram
    blankProgram = (M.fromList $ zip [0..m] (repeat 0))
    m = fst $ M.findMax (vm' ^. prog)


runOutput :: MapVM -> [Int] -> (ExitCode, MapVM, [Int])
runOutput vm inputs = runRWS executeToOutput inputs vm


-- program parser
programP :: Parser Program
programP = sepBy integer (char ',')


programFromString :: String -> Maybe Program
programFromString s = do
    let result = runParser programP "" s
    case result of
        (Left err) -> Nothing
        (Right r)  -> Just r


-- pretty printing programs
-- prettyFormat :: Program -> [String]
-- prettyFormat prog = helper $ V.toList prog where
--     helper :: [Int] -> [String]
--     helper [] = []
--     helper ls = line : helper rest where
--         (op, _) = opMap M.! head ls
--         (thisOp, rest) = splitAt (op ^. width) ls
--         thisOpStr = init . tail . show $ thisOp
--         line = show (op ^. name) ++ "\t\t" ++ thisOpStr
-- 
-- 
-- prettyPrint :: Program -> IO ()
-- prettyPrint prog = do 
--     let ls = prettyFormat prog
--     putStrLn $ unlines ls



-- next step is perhaps better use of monads
-- maybe some error handling for when all these lookups start going wrong
-- better tracking of the output, maybe with a writer or some other state?
-- so that you have programmatic access to the outputs
-- also streams so that you have programmatic control over the input
-- make a unified test so you don't have to go back and recompile all the old ones?
-- generalize the parsing and printing
-- separate out the Internal and External VM interfaces...
-- is external a typeclass? what is it
-- the list of values is a bit hack
-- PRETTY PRINTING IS JUST ANOTHER INTERFACE!
-- passing the Maybe Int around for the processing is a bit clunky
-- while it would be nice to distinguish Words and Ints, the libraries are not set up to handle it well :(
--      for the moment let's just let it all be ints
-- change the OpType to OpPacket with some of the types having data


-- want to be able to run in programmatic mode or debug mode or interactive mode
-- return to more lens usage
-- is there a better solution than pipes, just using RWS?