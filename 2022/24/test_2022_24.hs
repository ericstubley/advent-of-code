import Test.HUnit
import Puzzle_2022_24
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testFindGoal = TestCase $ do
    (Just storm) <- parseInput stormP "24/test_input.txt"
    18 @=? findGoal storm


testA = TestList
    [ TestLabel "Find goal" testFindGoal]

testB = TestList
    []

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()