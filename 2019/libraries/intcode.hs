-- what's the basic architecture?
-- you read a basic list of ints, pass it to execute
-- the main execution loop is to
--      read what op the instruction pointer is telling us to do
--      call the appropriate modification function
--      do that in a (tail-recursive) loop until you hit an exit


module Intcode ( Program
               , programP
               , programFromString
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

data Intcode s = Intcode { _iptr :: Int
                         , _prog :: MProgram s}


data OP = OP { _code :: Int
             , _width :: Int
             , _name :: String
             , _call :: MProgram s -> MProgram s}

makeLenses ''Intcode
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


-- top level executing programs
-- this is the function to call; the job of execute is to construct the
-- MProgram with pointer and pass to the executioner
execute :: Program -> Program
execute program = evalState go initial where
    initial = Intcode 0 (VM.thaw program)
    go 





# executioner :: PrimMonad m => Intcode (PrimState m) -> m (Intcode (PrimState m))
# executioner ic = do
#     ptrCode <- VM.read (prog ^. ic) (iptr ^. ic)
#     let op = M.! opLookup ptrCode
#     if name ^. op == "Exit"
#         then return ic
#         else do
#             ic' <- (call ^. op) ic
#             executioner ic'



-- op definitions, lookup, call functions

add :: OP
add = OP 1 4 "Add" opAdd

mul :: OP
mul = OP 2 4 "Mul" opMul

exit :: OP
exit = OP 99 1 "Exit" id


opAdd :: Program -> Int -> Program
opAdd p i = p V.// [(z, x+y)]
  where x = p V.! (p V.! (i+1))
        y = p V.! (p V.! (i+2))
        z = p V.! (i+3)

opMul :: Program -> Int -> Program
opMul p i = p V.// [(z, x*y)]
  where x = p V.! (p V.! (i+1))
        y = p V.! (p V.! (i+2))
        z = p V.! (i+3)



-- eventually you're gonna want
-- opcode lookup (i.e. for # of fields)
-- mutable programs?
-- pretty printing