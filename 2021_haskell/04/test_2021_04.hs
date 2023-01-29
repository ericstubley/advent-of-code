import Test.HUnit
import Puzzle_2021_04
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testFirstScore = TestCase $ do
    (Just (calls, boards)) <- parseInput bingoP "04/test_input.txt"
    4512 @=? firstScore calls boards 

testLastScore = TestCase $ do
    (Just (calls, boards)) <- parseInput bingoP "04/test_input.txt"
    1924 @=? lastScore calls boards 


testA = TestList
    [ TestLabel "First Score" testFirstScore]

testB = TestList
    [ TestLabel "Last Score" testLastScore]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()