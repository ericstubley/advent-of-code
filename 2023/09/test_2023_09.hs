import Test.HUnit
import Puzzle_2023_09
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testPredictions = TestCase $ do
    (Just report) <- parseInput reportP "09/test_input.txt"
    [18, 28, 68] @=? map prediction report


testExtrapolations = TestCase $ do
    (Just report) <- parseInput reportP "09/test_input.txt"
    [-3, 0, 5] @=? map extrapolation report


testA = TestList
    [ TestLabel "Predictions" testPredictions]

testB = TestList
    [ TestLabel "Extrapolations" testExtrapolations]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()