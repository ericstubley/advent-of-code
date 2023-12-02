import Test.HUnit
import Puzzle_2023_01
import Parsing

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testParseA = TestCase $ do
    (Just calibrationValues) <- parseInput simpleCalibrationsP "01/test_input.txt" 
    [12, 38, 15, 77] @=? calibrationValues


testOverlap = TestCase $ do
    (Just results) <- parseInput calibrationsP "01/overlap_test.txt"
    [98, 82, 18, 12] @=? results


testParseB = TestCase $ do
    (Just calibrationValues) <- parseInput calibrationsP "01/test_input_2.txt" 
    [29, 83, 13, 24, 42, 14, 76] @=? calibrationValues


testA = TestList
    [ TestLabel "Parsing A" testParseA]

testB = TestList
    [ TestLabel "Overlap edge case" testOverlap
    , TestLabel "Parsing B" testParseB]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()