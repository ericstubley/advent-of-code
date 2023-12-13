import Test.HUnit
import Puzzle_2023_13
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testSummarizeA = TestCase $ do
    (Just patterns) <- parseInput patternsP "13/test_input.txt"
    405 @=? summarizeA patterns

testSummarizeB = TestCase $ do
    (Just patterns) <- parseInput patternsP "13/test_input.txt"
    400 @=? summarizeB patterns


testA = TestList
    [ TestLabel "Summarize" testSummarizeA]

testB = TestList
    [ TestLabel "Summarize" testSummarizeB]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()