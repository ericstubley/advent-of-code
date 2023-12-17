import Test.HUnit
import Puzzle_2023_15
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testHash = TestCase $ do
   52 @=? hash "HASH" 

testHashLonger = TestCase $ do
    (Just input) <- parseInput inputP "15/test_input.txt"
    [30, 253, 97, 47, 14, 180, 9, 197, 48, 214, 231] @=? map hash input

testFocusingPower = TestCase $ do
    (Just steps) <- parseInput stepsP "15/test_input.txt"
    145 @=? (configPower . fillBoxes $ steps)

testA = TestList
    [ TestLabel "Hash" testHash
    , TestLabel "Longer hash" testHashLonger]

testB = TestList
    [ TestLabel "Focusing Power" testFocusingPower]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()