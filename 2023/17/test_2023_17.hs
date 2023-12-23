import Test.HUnit
import Puzzle_2023_17
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testHeatLossA = TestCase $ do
    (Just env) <- parseInput envP "17/test_input.txt"
    102 @=? heatLossA env

testHeatLossB = TestCase $ do
    (Just env1) <- parseInput envP "17/test_input.txt"
    -- print $ heatLossB env1
    94 @=? heatLossB env1
    (Just env2) <- parseInput envP "17/test_input_2.txt"
    -- print $ heatLossB env2
    71 @=? heatLossB env2

testA = TestList
    [ TestLabel "Heat loss A" testHeatLossA]

testB = TestList
    [ TestLabel "Heat loss B" testHeatLossB]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()