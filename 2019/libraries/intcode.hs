module Intcode ( Program
               , programP
               , programFromString
               , prettyFormat
               , prettyPrint
               , execute) where


import Data.Maybe
import Parsing (Parser, runParser, sepBy, integer, char)
import Data.Map.Strict (Map)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed.Mutable (IOVector)
import Lens.Micro.Platform
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

-- data types
type Program = Vector Int
type MProgram = IOVector Int

data OPName = Addition
            | Multiplication
            | Input
            | Output
            | JumpIfTrue
            | JumpIfFalse
            | LessThan
            | Equals
            | Exit deriving (Eq, Show)

data Mode = Position
          | Immediate deriving (Eq, Show)

data OP = OP 
    { _code :: Int
    , _name :: OPName
    , _width :: Int} deriving (Eq, Show)

makeLenses ''OP


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




-- top level executing programs
-- this is basically a wrapper around runST to pass to the executioner
-- needs to actually be ST and not State for actual legit mutation
execute :: Program -> IO Program
execute program = do
    mProgram  <- V.thaw program
    executioner mProgram 0
    V.freeze mProgram



-- there's an input which is the instruction pointer
-- the state is the current program
executioner :: MProgram -> Int -> IO ()
executioner prog iptr = do
    ptrValue <- VM.read prog iptr
    let opCode = mod ptrValue 100
    let (op, instruction) = opMap M.! opCode
    if op ^. name == Exit
        then return ()
        else do
            result <- instruction prog iptr
            let iptr' = case result of
                                Nothing -> iptr + (op ^. width)
                                (Just jump) -> jump
            executioner prog iptr'


-- extract the arg+2 digit (so hundreds for arg=1, thousands for arg=2)
-- convert it to mode
mode :: MProgram -> Int -> Int -> IO Mode
mode prog iptr arg = do
    ptrValue <- VM.read prog iptr
    let digit = div (mod ptrValue (10^(arg+2))) (10^(arg+1))
    case digit of
        0 -> return Position
        1 -> return Immediate
        _ -> error "Invalid mode"


value :: MProgram -> Int -> Int -> IO Int
value prog iptr arg = do
    m <- mode prog iptr arg
    case m of
        Position  -> (VM.read prog (iptr+arg)) >>= (VM.read prog)
        Immediate -> VM.read prog (iptr+arg)



-- op lookup and instruction functions

opMap :: Map Int (OP, MProgram -> Int -> IO (Maybe Int))
opMap = M.fromList
    [ ( 1, (op1, addition))
    , ( 2, (op2, multiplication))
    , ( 3, (op3, input))
    , ( 4, (op4, output))
    , ( 5, (op5, jumpIfTrue))
    , ( 6, (op6, jumpIfFalse))
    , ( 7, (op7, lessThan))
    , ( 8, (op8, equals))
    , (99, (op99, undefined))] 


op1 :: OP
op1 = OP 1 Addition 4

op2 :: OP
op2 = OP 2 Multiplication 4

op3 :: OP
op3 = OP 3 Input 2

op4 :: OP
op4 = OP 4 Output 2

op5 :: OP
op5 = OP 5 JumpIfTrue 3

op6 :: OP
op6 = OP 6 JumpIfFalse 3

op7 :: OP
op7 = OP 7 LessThan 4

op8 :: OP
op8 = OP 8 Equals 4

op99 :: OP
op99 = OP 99 Exit 1


addition :: MProgram -> Int -> IO (Maybe Int)
addition prog iptr = do
    val1 <- value prog iptr 1
    val2 <- value prog iptr 2
    idx3 <- VM.read prog (iptr+3)
    VM.write prog idx3 (val1 + val2)
    return Nothing


multiplication :: MProgram -> Int -> IO (Maybe Int)
multiplication prog iptr = do
    val1 <- value prog iptr 1
    val2 <- value prog iptr 2
    idx3 <- VM.read prog (iptr+3)
    VM.write prog idx3 (val1 * val2)
    return Nothing


input :: MProgram -> Int -> IO (Maybe Int)
input prog iptr = do
    idx <- VM.read prog (iptr+1)
    putStrLn "input:"
    inp <- getLine
    let val = (read inp :: Int)
    VM.write prog idx val
    return Nothing


output :: MProgram -> Int -> IO (Maybe Int)
output prog iptr = do
    val <- value prog iptr 1
    print val
    return Nothing


jumpIfTrue :: MProgram -> Int -> IO (Maybe Int)
jumpIfTrue prog iptr = do
    val <- value prog iptr 1
    if val /= 0
        then do
            iptr' <- value prog iptr 2
            return $ Just iptr'
        else return Nothing


jumpIfFalse :: MProgram -> Int -> IO (Maybe Int)
jumpIfFalse prog iptr = do
    val <- value prog iptr 1
    if val == 0
        then do
            iptr' <- value prog iptr 2
            return $ Just iptr'
        else return Nothing


lessThan :: MProgram -> Int -> IO (Maybe Int)
lessThan prog iptr = do
    val1 <- value prog iptr 1
    val2 <- value prog iptr 2
    idx3 <- VM.read prog (iptr+3)
    if val1 < val2
        then VM.write prog idx3 1
        else VM.write prog idx3 0
    return Nothing


equals :: MProgram -> Int -> IO (Maybe Int)
equals prog iptr = do
    val1 <- value prog iptr 1
    val2 <- value prog iptr 2
    idx3 <- VM.read prog (iptr+3)
    if val1 == val2
        then VM.write prog idx3 1
        else VM.write prog idx3 0
    return Nothing


-- next step is perhaps better use of monads
-- make the execution types polymorphic instead of restricting to ST
-- probably eventually you're gonna want IO in the mix
-- maybe some error handling for when all these lookups start going wrong
-- better tracking of the output, maybe with a writer or some other state?
-- so that you have programmatic access to the outputs
-- also streams so that you have programmatic control over the input