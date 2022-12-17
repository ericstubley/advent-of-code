import Test.HUnit
import Puzzle_2022_16
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testMaximumRelease = TestCase $ do
    (Just (adjacencies, rates)) <- parseInput graphP "16/test_input.txt"
    1651 @=? maximumRelease adjacencies rates


testCollaborativeRelease = TestCase $ do
    (Just (adjacencies, rates)) <- parseInput graphP "16/test_input.txt"
    1707 @=? collaborativeRelease adjacencies rates


testA = TestList
    [ testMaximumRelease]

testB = TestList
    [ testCollaborativeRelease]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()