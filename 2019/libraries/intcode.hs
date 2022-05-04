module Intcode ( Program
               , programP
               , programFromString
               , prettyFormat
               , prettyPrint
               , execute) where


import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Primitive
import Data.Maybe
import Parsing (Parser, runParser, sepBy, integer, char)
import Data.Map.Strict (Map)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed.Mutable (MVector)
import Lens.Micro.Platform
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

-- data types
type Program = Vector Int
type MProgram s = MVector s Int

data OPCode = Add
            | Mul
            | Exit deriving (Eq, Show)

data OP = OP 
    { _num :: Int
    , _code :: OPCode
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
        (op, _) = opLookup M.! head ls
        (thisOp, rest) = splitAt (op ^. width) ls
        thisOpStr = init . tail . show $ thisOp
        line = show (op ^. code) ++ "\t\t" ++ thisOpStr


prettyPrint :: Program -> IO ()
prettyPrint prog = do 
    let ls = prettyFormat prog
    putStrLn $ unlines ls




-- top level executing programs
-- this is basically a wrapper around runST to pass to the executioner
-- needs to actually be ST and not State for actual legit mutation
execute :: Program -> Program
execute program = runST $ do
    mProgram  <- V.thaw program
    executioner mProgram 0
    V.freeze mProgram



-- there's an input which is the instruction pointer
-- the state is the current program
executioner :: MProgram s -> Int -> ST s ()
executioner prog iptr = do
    opNum <- VM.read prog iptr
    let (op, instruction) = opLookup M.! opNum
    if op ^. code == Exit
        then return ()
        else do
            instruction prog iptr
            let iptr' = iptr + (op ^. width)
            executioner prog iptr'



-- op lookup and instruction functions

opLookup :: Map Int (OP, MProgram s -> Int -> ST s())
opLookup = M.fromList
    [ ( 1, (OP  1 Add  4, add))
    , ( 2, (OP  2 Mul  4, mul))
    , (99, (OP 99 Exit 1, undefined))]


add :: MProgram s -> Int -> ST s ()
add prog iptr = do
    idx1 <- VM.read prog (iptr+1)
    idx2 <- VM.read prog (iptr+2)
    idx3 <- VM.read prog (iptr+3)
    val1 <- VM.read prog idx1
    val2 <- VM.read prog idx2
    VM.write prog idx3 (val1 + val2)


mul :: MProgram s -> Int -> ST s ()
mul prog iptr = do
    idx1 <- VM.read prog (iptr+1)
    idx2 <- VM.read prog (iptr+2)
    idx3 <- VM.read prog (iptr+3)
    val1 <- VM.read prog idx1
    val2 <- VM.read prog idx2
    VM.write prog idx3 (val1 * val2)


-- next step is perhaps better use of monads
-- make the execution types polymorphic instead of restricting to ST
-- probably eventually you're gonna want IO in the mix
-- maybe some error handling for when all these lookups start going wrong