module Intcode ( Program
               , programP
               , programFromString
               , prettyFormat
               , prettyPrint
               , execute
               , runProgram) where


import Control.Monad.RWS
import Data.Maybe
import Data.Word
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
    vmCurr   :: m Word
    vmRead   :: m Int
    vmSeek   :: Word -> m ()
    vmPeek   :: Word -> m Int
    vmWrite  :: Word -> Int -> m()
    vmInput  :: m Int
    vmOutput :: Int -> m ()


-- immutable vector memory and RWS Interface
-- RWS used for Input (Reader), Output (Writer), VectorVM (State)
-- the inputPtr is a bit of a hack right now, just so you can get
--      something running without pipes
type Program = Vector Int

data VectorVM = VectorVM 
    { _iptr :: Word
    , _prog :: Program
    , _inputPtr :: Word} deriving (Eq, Show)
makeLenses ''VectorVM


instance Monad m => VMInterface (RWST [Int] [Int] VectorVM m) where
    vmCurr = gets iptr
    vmRead = do
        vm <- get
        let pos = vm ^. iptr
        put $ vm +~ iptr -- +~ is the "increment" lens operation
        return $ vm ^. prog . (ix pos)
    vmSeek pos = do
        vm <- get
        put $ vm & iptr .~ pos 
    vmPeek pos = do
        vm <- get
        return $ vm ^. prog . (ix pos)
    vmWrite pos reg = do
        vm <- get
        put $ vm & prog . (ix pos) .~ reg
    vmInput = do
        pos <- gets
        asks $ ( !! pos)
    vmOutput val = do
        tell [val]



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

data OpType = OpWrite
            | OpJump
            | OpNone
            | OpHalt deriving (Eq, Ord, Show)

data Mode = Pos
          | Imm deriving (Eq, Ord, Show)

-- what's the monadic pipeline for running programs?
-- a single step is
-- extract the current reg via a read
--      if we're incrementing the iptr this is our only chance to read modes etc.
-- split into modes and instruction with divMod
-- all the op functions take in a mode block
-- perform the op
-- do some post-processing: the common write pattern, jumping, halting

step :: (VMInterface m) => m Bool
step = selectOp >>= performOp >>= processOp


selectOp :: (VMInterface m) => m (OpName, Int)
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
        99 -> Hlt
        _  -> error $ "Unknown opcode " ++ show opNum
    return (op, mn)


performOp :: (VMInterface m) => (OpName, Int) -> m (OpType, Maybe Int) 
performOp (op, mn) = case op of
    Add -> do
        (x:y:[]) <- values 2 mn
        return (OpWrite, Just $ x+y)
    Mul -> do
        (x:y:[]) <- values 2 mn
        return (OpWrite, Just $ x*y)
    Get -> do
        inp <- vmInput
        return (OpWrite, Just inp)
    Put -> do
        (x:[]) <- values 1 mn
        vmOuput x
        return (OpNone, Nothing)
    Jnz -> do
        (x:y:[]) <- values 2 mn
        if x /= 0
            then return (OpJump, Just y)
            else return (OpNone, Nothing)
    Jez -> do
        (x:y:[]) <- values 2 mn
        if x == 0
            then return (OpJump, Just y)
            else return (OpNone, Nothing)
    Clt -> do
        (x:y:[]) <- values 2 mn
        if x < y
            then return (OpWrite, Just 1)
            else return (OpWrite, Just 0)
    Ceq -> do
        (x:y:[]) <- values 2 mn
        if x == y
            then return (OpWrite, Just 1)
            else return (OpWrite, Just 0)
    Hlt -> return (OpHalt, Nothing)


processOp :: (VMInterface m) => (OpType, Maybe Int) -> m Bool
processOp (op, queued) = case op of
    OpWrite -> do
        pos <- vmRead
        val <- queued
        vmWrite pos val
        return True
    OpJump  -> do
        val <- queued
        vmSeek $ fromIntegral val :: Word
        return True
    OpNone  -> return True
    OpHalt  -> return False


mode :: Int -> [Mode]
mode mn = case (mod mn 10) of
    0 -> Pos
    1 -> Imm
    _ -> error $ "Unknown mode " ++ show (mod mn 10)


values :: (VMInterface m) => Int -> Int -> m [Int]
values 0 _ = return []
values n mn = do
    val <- value (mode mn) 
    rest <- values (n-1) (div mn 10)
    return $ val : rest


value :: (VMInterface m) => Mode -> m Int
value m = case m of
    Pos -> vmRead >>= (vmPeek . fromIntegral)
    Imm -> vmRead


-- control flow of vm
-- need an execute
-- execute in interactive mode would be a nice option?

execute :: (VMInterface m) => m ()
execute = do
    halt <- step 
    if halt then return
            else execute


runProgram :: Program -> [Int] -> (Program, [Int])
runProgram program inputs = execRWS execute inputs (VectorVM 0 program 0)


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
prettyFormat :: Program -> [String]
prettyFormat prog = helper $ V.toList prog where
    helper :: [Int] -> [String]
    helper [] = []
    helper ls = line : helper rest where
        (op, _) = opMap M.! head ls
        (thisOp, rest) = splitAt (op ^. width) ls
        thisOpStr = init . tail . show $ thisOp
        line = show (op ^. name) ++ "\t\t" ++ thisOpStr


prettyPrint :: Program -> IO ()
prettyPrint prog = do 
    let ls = prettyFormat prog
    putStrLn $ unlines ls



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