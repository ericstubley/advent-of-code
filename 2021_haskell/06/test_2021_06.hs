import Test.HUnit
import Puzzle_2021_06
import Parsing (parseInput)
import Data.Vector (toList)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testDays = TestCase $ do
    (Just lanternfish) <- parseInput lanternfishP "06/test_input.txt"
    [0, 1, 1, 2, 1, 0, 0, 0, 0] @=? (toList $ days 0 lanternfish)
    [1, 1, 2, 1, 0, 0, 0, 0, 0] @=? (toList $ days 1 lanternfish)
    [1, 2, 1, 0, 0, 0, 1, 0, 1] @=? (toList $ days 2 lanternfish)
    [2, 1, 0, 0, 0, 1, 1, 1, 1] @=? (toList $ days 3 lanternfish)
    [1, 0, 0, 0, 1, 1, 3, 1, 2] @=? (toList $ days 4 lanternfish)
    [0, 0, 0, 1, 1, 3, 2, 2, 1] @=? (toList $ days 5 lanternfish)
    [0, 0, 1, 1, 3, 2, 2, 1, 0] @=? (toList $ days 6 lanternfish)
    [0, 1, 1, 3, 2, 2, 1, 0, 0] @=? (toList $ days 7 lanternfish)
    [1, 1, 3, 2, 2, 1, 0, 0, 0] @=? (toList $ days 8 lanternfish)
    [1, 3, 2, 2, 1, 0, 1, 0, 1] @=? (toList $ days 9 lanternfish)


testA = TestList
    [ TestLabel "Days" testDays]

testB = TestList
    []

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()