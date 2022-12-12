import Test.HUnit
import Puzzle_2022_12
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testShortestPath = TestCase $ do
    (Just area) <- parseInput areaP "12/test_input.txt"
    31 @=? shortestPath area


testShortestPath' = TestCase $ do
    (Just area) <- parseInput areaP "12/test_input.txt"
    29 @=? shortestPath' area


testA = TestList
    [ testShortestPath]

testB = TestList
    [ testShortestPath']

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()