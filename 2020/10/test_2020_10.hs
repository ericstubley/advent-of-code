import Test.HUnit
import Puzzle_2020_10
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testJoltageDiffs :: Test
testJoltageDiffs = TestCase $ do
    (Just adapters1) <- parseInput adaptersP "10/test_input_1.txt"
    (Just adapters2) <- parseInput adaptersP "10/test_input_2.txt"
    35  @=? joltageDiffs adapters1
    220 @=? joltageDiffs adapters2


testCountChains :: Test
testCountChains = TestCase $ do
    (Just adapters1) <- parseInput adaptersP "10/test_input_1.txt"
    (Just adapters2) <- parseInput adaptersP "10/test_input_2.txt"
    8 @=? countChains adapters1
    19208 @=? countChains adapters2



testA = TestList
    [ TestLabel "Joltage Diffs" testJoltageDiffs]

testB = TestList
    [ TestLabel "Count Chains" testCountChains]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()