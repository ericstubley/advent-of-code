import Test.HUnit
import Puzzle_2023_04
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testScores = TestCase $ do
    (Just cards) <- parseInput cardsP "04/test_input.txt"
    [8, 2, 2, 1, 0, 0] @=? map score cards


testCountCards = TestCase $ do
    (Just cards) <- parseInput cardsP "04/test_input.txt"
    [1, 2, 4, 8, 14, 1] @=? countCards cards

testA = TestList
    [ TestLabel "Scores" testScores]

testB = TestList
    [ TestLabel "Counting cards" testCountCards]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()