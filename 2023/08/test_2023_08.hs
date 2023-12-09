import Test.HUnit
import Puzzle_2023_08
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
testStepCount = TestCase $ do
    (Just environment) <- parseInput inputP "08/test_input.txt"
    2 @=? stepCount environment
    (Just environment) <- parseInput inputP "08/test_input_2.txt"
    6 @=? stepCount environment

testParallelStepCount = TestCase $ do
    (Just environment) <- parseInput inputP "08/test_input_3.txt"
    6 @=? ghostCount environment
    return ()



testA = TestList
    [ TestLabel "Step count" testStepCount]

testB = TestList
    [ TestLabel "Parallel step count" testParallelStepCount]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()