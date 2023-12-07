import Test.HUnit
import Puzzle_2023_07
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
testWinningsA = TestCase $ do
    (Just hands) <- parseInput handsPA "07/test_input.txt"
    6440 @=? winningsA hands

testWinningsB = TestCase $ do
    (Just hands) <- parseInput handsPB "07/test_input.txt"
    5905 @=? winningsB hands

testA = TestList
    [ TestLabel "WinningsA" testWinningsA]

testB = TestList
    [ TestLabel "WinningsB" testWinningsB]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()