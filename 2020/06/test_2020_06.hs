import Test.HUnit
import Puzzle_2020_06
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testScores :: Test
testScores = TestCase $ do
    (Just groups) <- parseInput groupsP "06/test_input.txt"
    [3, 3, 3, 1, 1] @=? map score groups


testScores' :: Test
testScores' = TestCase $ do
    (Just groups) <- parseInput groupsP "06/test_input.txt"
    [3, 0, 1, 1, 1] @=? map score' groups

testA = TestList
    [TestLabel "Scores" testScores]

testB = TestList
    [TestLabel "Scores'" testScores']

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()