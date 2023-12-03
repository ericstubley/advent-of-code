import Test.HUnit
import Puzzle_2023_03
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
testPartNumber = TestCase $ do
    (Just (numbers, symbols)) <- parseInput inputP "03/test_input.txt"
    [467, 35, 633, 617, 592, 755, 664, 598] @=? partNumbers numbers symbols
    4361 @=? sum (partNumbers numbers symbols)

testGearRatioSum = TestCase $ do
    (Just (numbers, symbols)) <- parseInput inputP "03/test_input.txt"
    467835 @=? gearRatioSum numbers symbols

testA = TestList
    [ TestLabel "Part number filtering" testPartNumber]

testB = TestList
    [ TestLabel "Gear ratio sum" testGearRatioSum]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()