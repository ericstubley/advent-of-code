module Intcode ( Program
               , VectorVM (..)
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
import Data.Vector.Unboxed (Vector)
import Lens.Micro.Platform
import qualified Data.Vector.Unboxed as V


-- abstract monadic interface for an Intcode VM
-- along with instances
-- currently supporting:
--      pure State monad vector
--      PrimMonad mutable vector
--      lift to Pipes?
--      lift to Exceptions?

class Monad m => VMInterface m where
    vmCurr   :: m Int
    vmRead   :: m Int
    vmSeek   :: Int -> m ()
    vmPeek   :: Int -> m Int        -- given an index, peek
    vmWrite  :: Int -> Int -> m()   -- given an index and a value
    vmInput  :: m Int
    vmOutput :: Int -> m ()


-- immutable vector memory and RWS Interface
-- RWS used for Input (Reader), Output (Writer), VectorVM (State)
-- the inputPtr is a bit of a hack right now, just so you can get
--      something running without pipes
type Program = Vector Int

data VectorVM = VectorVM 
    { _iptr :: Int
    , _prog :: Program
    , _inputPtr :: Int} deriving (Eq, Show)
makeLenses ''VectorVM


instance Monad m => VMInterface (RWST [Int] [Int] VectorVM m) where
    vmCurr = do
        vm <- get
        return $ vm ^. iptr
    vmRead = do
        vm <- get
        let pos = vm ^. iptr
        put $ vm & iptr .~ (pos + 1)
        return $ (vm ^. prog) V.! pos
    vmSeek pos = do
        vm <- get
        put $ vm & iptr .~ pos 
    vmPeek pos = do
        vm <- get
        return $ (vm ^. prog) V.! pos
    vmWrite pos val = do
        vm <- get
        let p' = (vm ^. prog) V.// [(pos, val)]
        put $ vm & prog .~ p'
    vmInput = do
        vm <- get
        let pos = vm ^. inputPtr
        put $ vm & inputPtr .~ (pos + 1)
        asks $ ( !! pos)
    vmOutput val = tell [val]



-- basic data enums for Ops and Modes

data OpName = Add
            | Mul
            | Get
            | Put
            | Jnz
            | Jez 
            | Clt
            | Ceq
            | Hlt deriving (Eq, Ord, Show)

data OpPacket = OpWrite Int
              | OpJump Int
              | OpOutput
              | OpNone
              | OpHalt deriving (Eq, Ord, Show)

data Mode = Pos
          | Imm deriving (Eq, Ord, Show)

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
step = selectOp >>= performOp >>= processOp


selectOp :: (VMInterface m) => m (OpName, Int)
selectOp = do
    reg <- vmRead
    let (mn, opNum) = divMod reg 100
    let op = case opNum of
                1 -> Add
                2 -> Mul
                3 -> Get
                4 -> Put
                5 -> Jnz
                6 -> Jez
                7 -> Clt
                8 -> Ceq
                99 -> Hlt
                _  -> error $ "Unknown opcode " ++ show opNum
    return (op, mn)


performOp :: (VMInterface m) => (OpName, Int) -> m OpPacket 
performOp (op, mn) = case op of
    Add -> do
        (x, y) <- values mn
        return $ OpWrite (x+y)
    Mul -> do
        (x, y) <- values mn
        return $ OpWrite (x*y)
    Get -> do
        inp <- vmInput
        return $ OpWrite inp
    Put -> do
        x <- value mn
        vmOutput x
        return OpOutput
    Jnz -> do
        (x, y) <- values mn
        if x /= 0
            then return $ OpJump y
            else return OpNone
    Jez -> do
        (x, y) <- values mn
        if x == 0
            then return $ OpJump y
            else return OpNone
    Clt -> do
        (x, y) <- values mn
        if x < y
            then return $ OpWrite 1
            else return $ OpWrite 0
    Ceq -> do
        (x, y) <- values mn
        if x == y
            then return $ OpWrite 1
            else return $ OpWrite 0
    Hlt -> return OpHalt


processOp :: (VMInterface m) => OpPacket -> m ExitCode
processOp op = case op of
    (OpWrite val) -> do
        pos <- vmRead
        vmWrite pos val
        return Continue
    (OpJump val) -> do
        vmSeek val
        return Continue
    OpOutput -> return Output
    OpNone -> return Continue
    OpHalt -> return Halt


mode :: Int -> Mode
mode mn = case (mod mn 10) of
    0 -> Pos
    1 -> Imm
    _ -> error $ "Unknown mode " ++ show (mod mn 10)


values :: (VMInterface m) => Int -> m (Int, Int)
values mn = do
    x <- value mn
    y <- value (div mn 10)
    return (x, y)


value :: (VMInterface m) => Int -> m Int
value mn = case (mode mn) of
    Pos -> vmRead >>= vmPeek
    Imm -> vmRead


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
    vm = VectorVM 0 program 0
    (vm', outputs) = execRWS execute inputs vm
    program' = vm' ^. prog


runOutput :: VectorVM -> [Int] -> (ExitCode, VectorVM, [Int])
runOutput vm inputs = runRWS executeToOutput inputs vm


-- program parser
programP :: Parser Program
programP = do
    ints <- sepBy integer (char ',')
    return $ V.fromList ints


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