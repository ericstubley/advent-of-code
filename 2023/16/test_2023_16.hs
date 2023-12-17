import Test.HUnit
import Puzzle_2023_16
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testCount = TestCase $ do
    (Just env) <- parseInput contraptionP "16/test_input.txt"
    46 @=? illuminated env

testMax = TestCase $ do
    (Just env) <- parseInput contraptionP "16/test_input.txt"
    51 @=? maxLumens env

testA = TestList
    [ TestLabel "Count" testCount]

testB = TestList
    [ TestLabel "Max" testMax]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()