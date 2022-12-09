import Test.HUnit
import Puzzle_2022_09
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testHeadTrack = TestCase $ do
    (Just instructions) <- parseInput instructionsP "09/test_input.txt"
    [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (4, 1)] @=? take 6 (headTrack instructions)

testTailTrack = TestCase $ do
    (Just instructions) <- parseInput instructionsP "09/test_input.txt"
    [(0, 0), (0, 0), (1, 0), (2, 0), (3, 0), (3, 0), (4, 1)] @=? take 7 (tailTrack instructions)

testCountTailPositions = TestCase $ do
    (Just instructions) <- parseInput instructionsP "09/test_input.txt"
    13 @=? countTailPositions instructions


testLong1 = TestCase $ do
    (Just instructions) <- parseInput instructionsP "09/test_input.txt"
    1 @=? countLongTailPositions 10 instructions


testLong2 = TestCase $ do
    (Just instructions) <- parseInput instructionsP "09/test_input_2.txt"
    36 @=? countLongTailPositions 10 instructions


testA = TestList
    [ testHeadTrack
    , testTailTrack
    , testCountTailPositions]

testB = TestList
    [ testLong1
    , testLong2]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()