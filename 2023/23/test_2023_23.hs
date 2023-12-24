import Test.HUnit
import Puzzle_2023_23
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testLongestPath = TestCase $ do
    (Just mountain) <- parseInput mountainP "23/test_input.txt"
    94 @=? longestPath mountain

testLongestPathB = TestCase $ do
    (Just mountain) <- parseInput mountainP "23/test_input.txt"
    154 @=? longestPathB mountain


testA = TestList
    [ TestLabel "Longest path" testLongestPath]

testB = TestList
    [ TestLabel "Longest path B" testLongestPathB]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()