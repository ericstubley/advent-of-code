module Intcode ( Program
               , MapVM (..)
               , ExitCode (..)
               , programP
               , programFromString
               -- , prettyFormat
               -- , prettyPrint
               , execute
               , runProgram
               , runInteractive) where
               -- , runOutput) where


import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Data.ByteString.Char8 (pack)
import Data.Conduino
import Data.Maybe
import Data.Void (Void)
import Parsing (Parser, runParser, sepBy, integer, char)
import Lens.Micro.Platform
import Data.IntMap.Strict (IntMap)
import qualified Data.Conduino.Combinators as C
import qualified Data.Conduino.Lift as L
import qualified Data.IntMap.Strict as M
-- import Data.Vector.Unboxed (Vector)
-- import qualified Data.Vector.Unboxed as V


-- abstract monadic interface for an Intcode VM
-- along with instances
-- currently supporting:
--      pure RWS monad intmap


class Monad m => VMInternalInterface m where
    vmCurr   :: m Index
    vmRead   :: m Int
    vmSeek   :: Index -> m ()
    vmPeek   :: Index -> m Int        -- given an index, peek
    vmWrite  :: Index -> Int -> m()   -- given an index and a value
    vmBase   :: m Index
    vmOffset :: Index -> m ()


class VMInternalInterface m => VMInterface m where
    vmInput  :: m (Maybe Int)
    vmOutput :: Int -> m ()


-- use this type for memory locations in intcode programs
-- just to help with conceptual clarity when i.e. reading type signatures
type Index = Int

-- everything that we export that deals with program should just treat them 
-- as lists, even though the internals may be very different
type Program = [Int]


-- immutable intmap memory and StateT Interface
-- to do this without pipes switch to RWST with inputPos
-- same strategy as when we did it for vectors

data MapVM = MapVM
    { iptr :: Index
    , base :: Index
    , prog :: IntMap Int}
    -- , inputPos :: Int}
    deriving (Eq, Show)


-- instance Monad m => VMInternalInterface (RWST [Int] [Int] MapVM m) where
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


-- instance Monad m => VMInterface (RWST [Int] [Int] MapVM m) where
--     vmInput = do
--         vm <- get
--         let pos = inputPos vm
--         put $ vm {inputPos = pos+1}
--         val <- asks $ ( !! pos)
--         return (Just val)
--     vmOutput val = tell [val]


instance VMInternalInterface m => VMInternalInterface (Pipe i o u m) where
    vmCurr = lift vmCurr
    vmRead = lift vmRead
    vmSeek pos = lift $ vmSeek pos
    vmPeek pos = lift $ vmPeek pos
    vmWrite pos val = lift $ vmWrite pos val
    vmBase = lift vmBase
    vmOffset val = lift $ vmOffset val


instance VMInternalInterface m => VMInterface (Pipe Int Int () m) where
    vmInput = await
    vmOutput val = yield val



-- basic data enums for Ops and Modes

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


-- what's the monadic pipeline for running programs?
-- a single step is
-- extract the current reg via a read
-- split into modes and instruction with divMod
-- all the op functions take in a mode block
-- perform the op
-- do some post-processing: the common write pattern, jumping, halting

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


-- this is the fixed interface that we export
-- as we change the monad stack around update this definition
runProgram :: Program -> [Int] -> (Program, [Int])
runProgram = runProgramPipeState


-- runProgramRWS :: Program -> [Int] -> (Program, [Int])
-- runProgramRWS program inputs = (program', outputs) where
--     vm = MapVM 0 0 (programToMap program) 0
--     (vm', outputs) = execRWS execute inputs vm
--     program' = mapToProgram (prog vm')


runProgramPipeState :: Program -> [Int] -> (Program, [Int])
runProgramPipeState program inputs = (program', outputs) where
    vm = MapVM 0 0 (programToMap program)
    (outputs, vm') = runState (programmaticPipeline inputs execute) vm
    program' = mapToProgram (prog vm')


programmaticPipeline :: 
    (VMInternalInterface m) => 
    [Int] -> Pipe Int Int () m () -> m [Int]
programmaticPipeline inputs executor = runPipe
    $ (C.sourceList inputs) .| executor .| C.sinkList


runInteractive :: Program -> IO ()
runInteractive program = runPipe
    $  C.stdinLines
    .| C.map (read :: String -> Int)
    .| L.execStateP vm execute
    .| C.mapM (print :: Int -> IO ())
    .| C.sinkNull
    where vm = MapVM 0 0 (programToMap program)


-- runOutput :: MapVM -> [Int] -> (ExitCode, MapVM, [Int])
-- runOutput vm inputs = runRWS executeToOutput inputs vm


programToMap :: Program -> IntMap Int
programToMap p = M.fromList (zip [0..] p)


mapToProgram :: IntMap Int -> Program
mapToProgram m = M.elems $ M.union m blankProgram
    where blankProgram = M.fromList $ zip [0..eom] (repeat 0)
          eom = fst $ M.findMax m


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