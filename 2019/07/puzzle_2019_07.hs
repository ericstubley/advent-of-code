module Puzzle_2019_07 where


import Data.List (permutations)
import Automation (submitAnswer)
import Intcode
import Parsing
import Lens.Micro.Platform


phaseThrust :: Program -> [Int] -> Int
phaseThrust program phases = go phases 0 where
    go :: [Int] -> Int -> Int
    go [] signal = signal
    go (p:ps) signal = go ps (head outputs) where
        (_, outputs) = runProgram program [p, signal]


feedbackThrust :: Program -> [Int] -> Int
feedbackThrust program phases = loop config 0
  where
    blankVM = VectorVM 0 program 0
    config = map (\p -> ([p], blankVM, [])) phases
    loop :: [([Int], VectorVM, [Int])] -> Int -> Int
    loop state i = case result of
        Output -> loop state' next
        Halt   -> if i == (l-1) then last outputs
                                else loop state' next
        where
            l = length phases
            (prev, next) = (mod (i-1) l, mod (i+1) l)
            (inputs, vm, outputs) = state !! i
            inputs' = inputs ++ [(last' 0) $ (state !! prev) ^. _3]
            (result, vm', outputs') = runOutput vm inputs'
            state' = replace i (inputs', vm', outputs ++ outputs') state


last' :: a -> [a] -> a
last' emptyDefault [] = emptyDefault
last' _ ls = last ls


replace :: Int -> a -> [a] -> [a]
replace i x xs = before ++ [x] ++ after
  where
    (before, cutoff) = splitAt i xs
    after = tail cutoff


maxThrust :: Program -> Int
maxThrust prog = maximum $ map (phaseThrust prog) phaseSpace
    where phaseSpace = permutations [0,1,2,3,4]


maxFeedbackThrust :: Program -> Int
maxFeedbackThrust prog = maximum $ map (feedbackThrust prog) phaseSpace
    where phaseSpace = permutations [5,6,7,8,9]


mainA :: IO ()
mainA = do
    (Just program) <- parseInput programP "07/input.txt"
    let answer = maxThrust program
    print answer
    -- result <- submitAnswer 2019 07 1 answer
    -- print result
    return ()


mainB :: IO ()
mainB = do
    (Just program) <- parseInput programP "07/input.txt"
    let answer = maxFeedbackThrust program
    result <- submitAnswer 2019 07 2 answer
    print result
    return ()
