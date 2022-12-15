import Test.HUnit
import Puzzle_2022_15
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testCountCoverage = TestCase $ do
    (Just pairs) <- parseInput pairsP "15/test_input.txt"
    26 @=? countCoverage 10 pairs


testTuningFrequency = TestCase $ do
    (Just pairs) <- parseInput pairsP "15/test_input.txt"
    56000011 @=? (tuningFrequency $ find 0 20 pairs)


testA = TestList
    [ testCountCoverage]

testB = TestList
    [ testTuningFrequency]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()