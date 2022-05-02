{-# LANGUAGE BangPatterns #-}

module Intcode ( Program
               , programP
               , programFromString
               , execute) where


import Data.Maybe
import Parsing (Parser, runParser, sepBy, integer, char)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

-- data types
type Program = Vector Int


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


-- execute program
execute :: Program -> Program
execute program = go program 0
  where go (!p) (!i) 
            | op == 1  = go (opAdd p i) (i+4)
            | op == 2  = go (opMul p i) (i+4)
            | op == 99 = p
            where op = p V.! i


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