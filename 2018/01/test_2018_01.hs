import Test.HUnit
import System.IO
import Puzzle_2018_01

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testFreqSum = TestCase (do
    3 @=? freqSum [1, -2, 3, 1]
    3 @=? freqSum [1, 1, 1]
    0 @=? freqSum [1, 1, -2]
    -6 @=? freqSum [-1, -2, -3])

testParse = TestCase (do
    freqs <- parseInput "test_input.txt"
    [1, -2, 3, 1] @=? freqs)

testFirstRepeat = TestCase (do
    2  @=? firstRepeat [1, -2, 3, 1]
    0  @=? firstRepeat [1, -1]
    10 @=? firstRepeat [3, 3, 4, -2, -4]
    5  @=? firstRepeat [-6, 3, 8, 5, -6]
    14 @=? firstRepeat [7, 7, -2, -7, -4])


testA = TestList
    [ TestLabel "Freq Sum" testFreqSum
    , TestLabel "Parsing" testParse]

testB = TestList
    [ TestLabel "FirstRepeat" testFirstRepeat]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()