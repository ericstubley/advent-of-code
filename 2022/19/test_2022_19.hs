import Test.HUnit
import Puzzle_2022_19
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
testParse = TestCase $ do
    (Just blueprints) <- parseInput blueprintsP "19/test_input.txt"
    print $ head blueprints
    print $ last blueprints


testMaximizeGeodes = TestCase $ do
    (Just blueprints) <- parseInput blueprintsP "19/test_input.txt"
    9 @=? maximizeGeodes (head blueprints) 24
    12 @=? maximizeGeodes (last blueprints) 24


testLongerGeodeTime = TestCase $ do
    (Just blueprints) <- parseInput blueprintsP "19/test_input.txt"
    56 @=? maximizeGeodes (head blueprints) 32
    62 @=? maximizeGeodes (last blueprints) 32


testA = TestList
    [ testParse
    , testMaximizeGeodes]

testB = TestList
    [ testLongerGeodeTime]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()