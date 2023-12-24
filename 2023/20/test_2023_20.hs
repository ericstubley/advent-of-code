import Test.HUnit
import Puzzle_2023_20
import Parsing (parseInput)
import Control.Monad.State

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
testDebug = TestCase $ do
    (Just modules) <- parseInput modulesP "20/test_input_3.txt"
    let env = buildEnv modules
    -- print env
    -- print $ execState push env
    return ()


testDebug2 = TestCase $ do
    -- (Just modules) <- parseInput modulesP "20/test_input_2.txt"
    -- let env = buildEnv modules
    -- let s0 = env
    -- let s1 = execState push s0
    -- let s2 = execState push s1
    -- let a1 = execState (enQueue >> repeatM 1 tick) s2
    -- let a2 = execState (enQueue >> repeatM 2 tick) s2
    -- let a3 = execState (enQueue >> repeatM 3 tick) s2
    -- let a4 = execState (enQueue >> repeatM 4 tick) s2
    -- let a5 = execState (enQueue >> repeatM 5 tick) s2
    -- let a6 = execState (enQueue >> repeatM 6 tick) s2
    -- let a7 = execState (enQueue >> repeatM 7 tick) s2
    -- let a8 = execState (enQueue >> repeatM 8 tick) s2
    -- print $ _queue s2
    -- print "\n"
    -- print $ _queue a1
    -- print "\n"
    -- print $ _queue a2
    -- print "\n"
    -- print $ _queue a3
    -- print "\n"
    -- print $ _queue a4
    -- print "\n"
    -- print $ _queue a5
    -- print "\n"
    -- print $ _queue a6
    -- print "\n"
    -- print $ _queue a7
    -- print "\n"
    -- print $ _queue a8
    -- print "\n"
    -- let s3 = execState push s2
    -- let s4 = execState push s3
    -- print ""
    -- print s0
    -- print ""
    -- print s1
    -- print ""
    -- print s2
    -- print ""
    -- print s3
    -- print ""
    -- print s4
    return ()



testPulseProduct = TestCase $ do
    (Just modules1) <- parseInput modulesP "20/test_input.txt"
    32000000 @=? solveA modules1
    (Just modules2) <- parseInput modulesP "20/test_input_2.txt"
    11687500 @=? solveA modules2


testA = TestList
    [ TestLabel "Debugging" testDebug
    , TestLabel "More debugging" testDebug2
    , TestLabel "Pulse Product" testPulseProduct]

testB = TestList
    []

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()