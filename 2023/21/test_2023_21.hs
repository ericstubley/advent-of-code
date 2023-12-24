import Test.HUnit
import Puzzle_2023_21
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
testStepLocations = TestCase $ do
    (Just garden) <- parseInput gardenP "21/test_input.txt"
    2 @=? steps garden 1
    4 @=? steps garden 2
    6 @=? steps garden 3
    16 @=? steps garden 6

testWrapSteps = TestCase $ do
    (Just garden) <- parseInput gardenP "21/test_input.txt"
    (Just garden) <- parseInput gardenP "21/test_input.txt"
    16 @=? wrapSteps garden 6
    50 @=? wrapSteps garden 10
    1594 @=? wrapSteps garden 50
    6536 @=? wrapSteps garden 100



testA = TestList
    [ TestLabel "Number of locations" testStepLocations]

testB = TestList
    [ TestLabel "Wrap steps" testWrapSteps]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()