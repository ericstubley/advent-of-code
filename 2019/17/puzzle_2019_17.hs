module Puzzle_2019_17 where

import Automation (submitAnswer)
import Grid
import Intcode
import Parsing
import Data.Char (chr, ord)
import Data.Conduino
import Data.Massiv.Array (Ix2(..), Sz(..), Border(..))
import qualified Data.Massiv.Array as A
import qualified Data.Conduino.Combinators as C
import Data.Massiv.Array.Stencil ( Stencil
                                 , Padding(..)
                                 , makeStencil
                                 , mapStencil)


trackMap :: Program -> Array2 Char
trackMap program = A.fromLists' A.Seq (lines $ init output) where
    (_, output) = runProgramAscii program []


intersectionStencil :: Stencil Ix2 Char Bool
intersectionStencil = makeStencil (Sz (3:.3)) (1:.1) go 
  where go get = all (\x -> get x =='#') nbhd


nbhd :: [Ix2]
nbhd = [0:.0, 0:.1, 0:.(-1), 1:.0, (-1):.0]


markIntersections :: Array2 Char -> Array2 Bool
markIntersections grid = A.computeAs A.U $ 
    A.mapStencil (Fill '.') intersectionStencil grid


alignmentParameters :: Array2 Char -> Array2 Int
alignmentParameters grid = A.computeAs A.U $ 
    A.imap parameter (markIntersections grid)
  where parameter :: Ix2 -> Bool -> Int
        parameter _ False = 0
        parameter (y:.x) True = x*y


manualInput :: String
manualInput = (unlines [main, a, b, c, vid]) ++ "\n" where
    main = "A,C,A,B,C,B,A,C,A,B"
    a    = "R,6,L,10,R,8,R,8"
    b    = "R,12,L,10,R,6,L,10"
    c    = "R,12,L,8,L,10"
    vid  = "n"


-- slap a pipe together to let us extract the final int without breaking chr
runBot :: Program -> String -> (String, Int)
runBot program input = (map chr $ init output, last output)
  where (_, output) = runPipePure
                         $ C.sourceList input
                        .| C.map ord
                        .| intcodePipe program
                        &| C.sinkList


mainA :: IO ()
mainA = do
    (Just rescueBot) <- parseInput programP "17/input.txt"
    let grid = trackMap rescueBot
    let answer = A.sum (alignmentParameters grid)
    print answer
    -- result <- submitAnswer 2019 17 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just rescueBot) <- parseInput programP "17/input.txt"
    let rescueBot' = [2] ++ tail rescueBot
    let (final, spaceDust) = runBot rescueBot' manualInput
    putStr final
    let answer = spaceDust
    print answer
    -- result <- submitAnswer 2019 17 2 answer
    -- print result
    return ()
