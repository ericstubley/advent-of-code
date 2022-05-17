module Puzzle_2019_21 where

import Automation (submitAnswer)
import Intcode
import Parsing
import Data.Char (ord)
import Data.Conduino
import qualified Data.Conduino.Combinators as C


programA :: String
programA = unlines 
    [ "OR A J"
    , "AND B J"
    , "AND C J"
    , "NOT J J"
    , "AND D J"
    , "WALK"]


programB :: String
programB = unlines
    [ "OR A J"
    , "AND B J"
    , "AND C J"
    , "NOT J J"
    , "AND D J"
    , "OR E T"
    , "OR H T"
    , "AND T J"
    , "RUN"]


runSpringDroid :: Program -> String -> Int
runSpringDroid program input = last output
  where (_, output) = runPipePure
                         $ C.sourceList input
                        .| C.map ord
                        .| intcodePipe program
                        &| C.sinkList

mainA :: IO ()
mainA = do
    (Just spring) <- parseInput programP "21/input.txt"
    let answer = runSpringDroid spring programA
    print answer
    -- result <- submitAnswer 2019 21 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just spring) <- parseInput programP "21/input.txt"
    let answer = runSpringDroid spring programB
    print answer
    -- result <- submitAnswer 2019 21 2 answer
    -- print result
    return ()
