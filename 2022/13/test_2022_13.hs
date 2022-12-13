import Test.HUnit
import Puzzle_2022_13
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testComparisons = TestCase $ do
    (Just packetPairs) <- parseInput packetPairsP "13/test_input.txt"
    [LT, LT, GT, LT, GT, LT, GT, GT] @=? comparisons packetPairs 


testCorrectIndices = TestCase $ do
    (Just packetPairs) <- parseInput packetPairsP "13/test_input.txt"
    [1, 2, 4, 6] @=? correctIndices packetPairs


testDecoderKey = TestCase $ do
    (Just packets) <- parseInput packetsP "13/test_input.txt"
    140 @=? decoderKey packets


testA = TestList
    [ testComparisons
    , testCorrectIndices]

testB = TestList
    [ testDecoderKey]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()